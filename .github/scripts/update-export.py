"""Refresh app sources while preserving an already verified Shinylive runtime."""
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import sys
import tarfile
import urllib.request

ROOT = Path(__file__).resolve().parents[2]
SITE = ROOT / "site"

def api(path):
    request = urllib.request.Request(
        "https://api.github.com/repos/" + os.environ["GITHUB_REPOSITORY"] + path,
        headers={"Authorization": "Bearer " + os.environ["GH_TOKEN"],
                 "Accept": "application/vnd.github+json"})
    with urllib.request.urlopen(request, timeout=60) as response:
        return json.load(response)

def find_base():
    for run in api("/actions/runs?status=success&per_page=20")["workflow_runs"]:
        artifacts = api("/actions/runs/" + str(run["id"]) + "/artifacts")["artifacts"]
        if any(a["name"] == "github-pages" and not a["expired"] for a in artifacts):
            with open(os.environ["GITHUB_ENV"], "a", encoding="utf-8") as env:
                env.write("BASE_RUN_ID=" + str(run["id"]) + "\n")
            print("Using verified export from run", run["id"])
            return
    raise RuntimeError("No retained runtime artifact; run the full-export workflow first.")

def dependencies(files):
    imports = set()
    for item in files:
        if item["name"].endswith(".R"):
            code = item["content"]
            imports.update(re.findall(r"(?:library|require|requireNamespace)\(\s*['\"]?([\w.]+)", code))
            imports.update(re.findall(r"\b([A-Za-z][\w.]*)::", code))
    return imports

def refresh():
    archive = ROOT / "baseline" / "artifact.tar"
    SITE.mkdir(exist_ok=True)
    with tarfile.open(archive) as source:
        source.extractall(SITE, filter="data")
    app_path = SITE / "app.json"
    files = json.loads(app_path.read_text(encoding="utf-8"))
    old_dependencies = dependencies(files)
    old_names = {item["name"] for item in files}
    source_names = {str(path.relative_to(ROOT / "app")).replace("\\", "/")
                    for path in (ROOT / "app").rglob("*") if path.is_file()}
    if old_names != source_names:
        raise RuntimeError("App file set changed; use the full-export workflow.")
    for item in files:
        if item["type"] != "text":
            raise RuntimeError("Non-text app asset requires full export.")
        path = (ROOT / "app" / item["name"]).resolve()
        if not path.is_relative_to(ROOT / "app"):
            raise RuntimeError("Invalid app path")
        item["content"] = path.read_text(encoding="utf-8")
    if not dependencies(files).issubset(old_dependencies):
        raise RuntimeError("New R package dependency requires full export.")
    app_path.write_text(json.dumps(files, ensure_ascii=False), encoding="utf-8")
    version = os.environ["GITHUB_SHA"][:12]
    loader_path = SITE / "shinylive" / "shinylive.js"
    loader = loader_path.read_text(encoding="utf-8")
    loader, count = re.subn(r'app\.json\?v=[A-Za-z0-9]+', "app.json?v=" + version, loader)
    if count == 0:
        raise RuntimeError("Expected versioned app JSON loader not found")
    loader_path.write_text(loader, encoding="utf-8")
    index_path = SITE / "index.html"
    index = index_path.read_text(encoding="utf-8")
    index = re.sub(r'shinylive\.js\?v=[A-Za-z0-9]+', "shinylive.js?v=" + version, index)
    index, count = re.subn(r'(<meta name="app-build" content=")[^"]+', r'\g<1>' + version, index)
    if count != 1:
        raise RuntimeError("Expected build marker not found")
    index_path.write_text(index, encoding="utf-8")
    for source, target in [("app/app.R", "Shiny_app.R"), ("app/stats.R", "stats.R"), ("analysis.R", "analysis.R")]:
        shutil.copyfile(ROOT / source, SITE / target)
    check = json.loads(app_path.read_text(encoding="utf-8"))
    for item in check:
        actual = (ROOT / "app" / item["name"]).read_text(encoding="utf-8")
        if item["content"] != actual:
            raise RuntimeError("Source verification failed: " + item["name"])
    print("Verified", len(check), "app files; runtime preserved; build", version)
    print("App JSON SHA-256:", hashlib.sha256(app_path.read_bytes()).hexdigest())

if __name__ == "__main__":
    find_base() if sys.argv[1:] == ["find-base"] else refresh()


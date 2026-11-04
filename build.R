# Export the complete Shiny R application for GitHub Pages.
options(repos=c(CRAN='https://cloud.r-project.org'))
if(!requireNamespace('shinylive',quietly=TRUE))stop('請先 install.packages("shinylive")')
# Only remove this project's generated export directory, never source/data.
project_root<-normalizePath(getwd(),winslash='/',mustWork=TRUE)
site_dir<-normalizePath(file.path(project_root,'site'),winslash='/',mustWork=FALSE)
stopifnot(identical(dirname(site_dir),project_root),basename(site_dir)=='site')
if(dir.exists(site_dir))unlink(site_dir,recursive=TRUE)
shinylive::export('app','site',wasm_packages=TRUE,template_params=list(title='平均數差異檢定｜邱皓政統計互動教材'))
dir.create('site/downloads',recursive=TRUE,showWarnings=FALSE)
file.copy(list.files('downloads',full.names=TRUE),'site/downloads',recursive=TRUE,overwrite=TRUE)
file.copy('analysis.R','site/analysis.R',overwrite=TRUE)
file.copy('app/app.R','site/Shiny_app.R',overwrite=TRUE)
file.copy('app/stats.R','site/stats.R',overwrite=TRUE)
writeLines('','site/.nojekyll')
page<-paste(readLines('site/index.html',warn=FALSE,encoding='UTF-8'),collapse='\n')
page<-sub('lang="en"','lang="zh-Hant"',page,fixed=TRUE)
# Version the loader and application JSON so updates do not reuse an older app.
version<-substr(Sys.getenv('GITHUB_SHA'),1,12)
if(!nzchar(version))version<-substr(system('git rev-parse HEAD',intern=TRUE)[1],1,12)
loader_path<-'site/shinylive/shinylive.js'
loader<-rawToChar(readBin(loader_path,'raw',n=file.info(loader_path)$size))
Encoding(loader)<-'UTF-8'
needle<-'fetch("./app.json")'
if(!grepl(needle,loader,fixed=TRUE))stop('Shinylive loader changed; review app JSON cache handling')
loader<-sub(needle,paste0('fetch("./app.json?v=',version,'", {cache: "no-store"})'),loader,fixed=TRUE)
writeLines(enc2utf8(loader),loader_path,useBytes=TRUE)
page<-sub('"./shinylive/shinylive.js"',paste0('"./shinylive/shinylive.js?v=',version,'"'),page,fixed=TRUE)
page<-sub('</head>',paste0('<meta name="app-build" content="',version,'"/></head>'),page,fixed=TRUE)
writeLines(enc2utf8(page),'site/index.html',useBytes=TRUE)
cat('Export complete: site/\n')

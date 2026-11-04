# 平均數差異檢定互動教材

邱皓政統計互動教材，延續「區間估計與假設檢定」，以 Shiny R / Shinylive 在瀏覽器中執行。

網站：https://hawjeng.github.io/mean-difference-lab/
教材入口：https://hawjeng.github.io/resources.html

## 教材內容

1. 原理與流程：獨立／配對設計、已知 σ 的 Z、等變異數 Student t、Welch t、配對 t、公式、假設與判讀。
2. 課本範例：研究生年齡、Uber 搭車、Levene 與 Brown–Forsythe，附作者提供的四份 Excel 原檔。
3. 調查研究：任選兩星座、性別、iPhone／非 iPhone、吃到飽／非吃到飽，對 13 個數值變數作比較。
4. 習作詳解：完整處理世代智力、補習成績、女性學者論文篇數三大題與各小題，可下載含圖的 HTML。
5. 自行輸入：可編輯兩組摘要、母體 σ 或配對相關，選擇 α、左尾／右尾／雙尾與非零虛無差異。

## 圖形與數值

橙色為事先設定的 α 拒絕域，藍色為觀察值與更極端值的 p-value 區域，分成同軸對齊的兩圖。橙色虛線為臨界值，紫色實線為 z_obt／t_obt。單尾依原假設方向計算，反向結果不取絕對值，也不把雙尾 p 一律除二。圖形數值使用 R 的 pnorm/pt/qnorm/qt；繪圖網格僅用於顯示面積。

差異固定為第一組減第二組。CI 固定為雙側 1−α，只有雙尾檢定可直接對照相同模型的 CI 是否包含 Δ₀。Welch 保留小數自由度，與 Excel 原檔取整 df 或預先四捨五入的結果可能略有不同。變異數檢定不作為強制選擇 t 模型的開關。

## 資料與公開範圍

以作者 2026-11-05 最新提供的 data2.xlsx / Cleaned_Data（C/Ch08）713 列為來源。公開檔僅含分組摘要，無逐筆 ID、學院、個性、其他文字回答或個人聯合紀錄，Word 校稿不公開。原始調查 Excel 不在 repository 中。

- 各數值變數各自排除非數值／缺失，保留 0，不重新刪除離群值。
- 星座使用 Z=1…12，性別使用 Gender。
- PhoneBrand=1 為 iPhone，2…10 為非 iPhone（含無手機），不明與其他排除。
- DataPlan=1 為吃到飽，2…4 為非吃到飽（含預付卡、Wi-Fi only），其他排除。
- 時間欄位與量尺單位不由欄名自行推定，請對照原問卷。
- 調查預設 Welch t，也可選獨立 Z 並自行給定目前變數的已知母體變異數 σ₁²、σ₂²。課本與自行輸入頁同樣直接填 σ²，不需自行開根號。Z 不將樣本 s² 當成已知 σ²，也不將同一組變異數套用到所有不同單位的變數。
- 13 變數總覽提供 Holm 校正；此家族不含使用者另外嘗試的其他組別或方向。
- 星座、手機等非隨機分派，不作因果推論。便利樣本推論及小樣本偏態限制於介面內說明。

`downloads/data-audit.json` 記錄來源雜湊，`downloads/survey-summary.csv` 提供重現用摘要。習作三將兩獨立組與「若是同一人前後測」分開，不擅自假定有配對。習作一若 16 是樣本 s，以 t 為主要答案，Z 僅列為已知 σ／大樣本教學對照。

如需由作者原始檔重新產生摘要，安裝 Python openpyxl，設定環境變數 `SURVEY_XLSX` 為調查檔位置、`CHAPTER_DATA_DIR` 為四份章節 Excel 所在資料夾，再執行 `python prepare_data.py`。這些原始檔位置不會寫入公開資料。

## 本機執行與驗證

```r
install.packages(c('shiny','jsonlite','shinylive'))
shiny::runApp('app')
source('analysis.R', encoding='UTF-8')
```

```sh
Rscript tests/statistical-checks.R
Rscript build.R
```

`tests/statistical-checks.R` 核對 R 原生 t.test、單尾反向、非零 Δ₀、變異數檢定、無效輸入及所有調查群組組合。GitHub Actions 通過後匯出並發布 GitHub Pages。下載的單支 R 程式需搭配 repository 的資料夾結構。

章節教材與作者原始資料的權利仍屬原權利人，未宣稱重新開放授權。

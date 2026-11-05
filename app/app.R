library(shiny)
source('stats.R',encoding='UTF-8',local=TRUE)
survey<-jsonlite::fromJSON('data/survey.json',simplifyVector=FALSE)
book<-jsonlite::fromJSON('data/textbook.json',simplifyVector=TRUE)
selectInput<-function(...)shiny::selectInput(...,selectize=FALSE)
tails<-c('雙尾：第一組 ≠ 第二組'='two','左尾：第一組 < 第二組'='left','右尾：第一組 > 第二組'='right')
methods<-c('Welch t（不假設等變異數）'='welch','Student t（等變異數）'='pooled','獨立樣本 Z（母體變異數 σ² 已知）'='z','配對 t（需配對資料）'='paired')
card<-function(...)div(class='lab-card',...)
hint<-function(...)p(class='hint',...)
diagrams<-function(r)tagList(fluidRow(column(6,div(class='diagram-scroll',HTML(distribution_svg(r)))),column(6,div(class='diagram-scroll',HTML(distribution_svg(r,TRUE))))),hint('兩圖使用相同橫軸。橙色虛線是臨界值，紫色實線是觀察檢定量。α 不是 H₀ 為真的機率，p-value 也不是 H₀ 為真的機率。'))
table_html<-function(df){heads<-paste0('<th>',esc(names(df)),'</th>',collapse='');rows<-vapply(seq_len(nrow(df)),function(i)paste0('<tr>',paste0('<td>',esc(unlist(df[i,],use.names=FALSE)),'</td>',collapse=''),'</tr>'),character(1));HTML(paste0('<div class="table-scroll"><table><thead><tr>',heads,'</tr></thead><tbody>',paste(rows,collapse=''),'</tbody></table></div>'))}
safe_ui<-function(expr)tryCatch(expr,error=function(e)div(class='warning',conditionMessage(e)))
show_result<-function(r,labels=c('第一組','第二組')){
 tail_label<-switch(r$tail,two='雙尾',left='左尾',right='右尾')
 tagList(div(class='hypothesis',p(hypothesis(r)),p(hypothesis(r,TRUE))),table_html(result_frame(list(r))),
  div(class=if(r$reject)'decision reject' else 'decision',if(r$reject)'拒絕 H₀' else '未能拒絕 H₀'),
  p(sprintf('%s，差異方向為「%s − %s」。觀察差異 = %s，標準誤 SE = %s，%s_obt = %s，p = %s。',tail_label,labels[1],labels[2],fmt(r$difference),fmt(r$se),r$distribution,fmt(r$stat),pfmt(r$p))),
  p(if(r$reject)'在指定顯著水準下，有證據支持所設定的對立假設；統計顯著不表示差異很大，也不表示因果關係。' else '資料尚不足以支持所設定的對立假設，不能解釋為已證明兩組相等。'),
  hint(sprintf('表中 CI 固定為雙側 %.1f%% 差異信賴區間。僅雙尾且使用相同模型時，CI 是否包含 Δ₀ 與 α 檢定相對應。',(1-r$alpha)*100)),
  if(r$method=='z')div(class='hypothesis',paste0('已知 σ₁² = ',fmt(r$sigma1^2),'，σ₂² = ',fmt(r$sigma2^2),'。SE = √(σ₁²/n₁ + σ₂²/n₂) = √(',fmt(r$sigma1^2),'/',r$a$n,' + ',fmt(r$sigma2^2),'/',r$b$n,') = ',fmt(r$se),'。輸入的是變異數（原始單位的平方），不是標準差。')),
  tags$details(tags$summary('五步驟與公式代入'),tags$ol(
   tags$li(paste('發展假設：',hypothesis(r),'；',hypothesis(r,TRUE))),
   tags$li(paste0('評估設計：',method_name(r$method),'。',if(r$method=='paired')paste0('n = ',r$a$n,' 對，r = ',fmt(r$r),'。') else paste0('n₁ = ',r$a$n,if(!is.null(r$b))paste0('，n₂ = ',r$b$n) else '', '。'))),
   tags$li(paste0('設定 α = ',r$alpha,'；',tail_label,'；',r$distribution,' 分配',if(is.finite(r$df))paste0('，df = ',fmt(r$df,3)) else '', '。')),
   tags$li(paste0('檢定量 = (差異 − Δ₀) / SE = (',fmt(r$difference),' − ',fmt(r$delta0),') / ',fmt(r$se),' = ',fmt(r$stat),'。')),
   tags$li(paste0('比較 p = ',pfmt(r$p),' 與 α = ',r$alpha,'，',if(r$reject)'拒絕 H₀。' else '未能拒絕 H₀。')))),diagrams(r))
}
example_choices<-c('Uber 搭車：獨立樣本 Z／Student／Welch'='uber','研究生年齡：獨立樣本比較'='age','Uber 搭車：同八人的配對 t'='paired','變異數同質檢驗：Levene 與 Brown–Forsythe'='levene')
ui<-fluidPage(tags$head(tags$title('平均數差異檢定｜邱皓政統計互動教材'),tags$link(rel='stylesheet',href='style.css'),tags$script(src='downloads.js')),
 div(class='app-top',a(href='https://hawjeng.github.io/resources.html','← 教學與專案'),a(href='https://hawjeng.github.io/statistical-inference-lab/','區間估計與假設檢定'),a(href='https://github.com/Hawjeng/mean-difference-lab',target='_blank',rel='noopener','GitHub 原始碼')),
 div(class='app-title',p(class='eyebrow','STATISTICS LAB · COMPARING TWO MEANS'),h1('平均數差異檢定'),p('先辨識研究設計，再看標準誤、拒絕域與尾機率。')),
 tabsetPanel(id='main_tab',selected='原理與流程',
  tabPanel('原理與流程',includeHTML('www/lesson.html')),
  tabPanel('課本範例',card(h3('從各節資料開始'),selectInput('example','範例主題',example_choices),
   div(class='book-controls',numericInput('ba','顯著水準 α',.05,min=.001,max=.2,step=.005),selectInput('bt','檢定方向',tails),numericInput('bd','虛無假設差異 Δ₀',0)),
   conditionalPanel("input.example != 'paired' && input.example != 'levene'",selectInput('bm','方法',methods[1:3]),conditionalPanel("input.bm == 'z'",div(class='book-controls',numericInput('bs1','第一組已知母體變異數 σ₁²',2,min=0),numericInput('bs2','第二組已知母體變異數 σ₂²',2,min=0)),hint('直接輸入變異數。例如 σ=2 時，這裡應輸入 σ²=4。兩組變異數可以不同。'))),
   uiOutput('book_question'),uiOutput('book_result'),tags$details(tags$summary('原始數據與試算表'),tableOutput('book_raw'),uiOutput('book_links')))),
  tabPanel('調查研究',card(h3('兩組平均數差異'),p('713 筆調查資料，以分組摘要在瀏覽器內計算。每次只比較兩個互斥群組。'),
   div(class='book-controls',selectInput('group','分組方式',setNames(names(survey$groups),vapply(survey$groups,function(g)g$label,character(1)))),selectInput('variable','分析變數',setNames(names(survey$variables),vapply(survey$variables,function(v)v$label,character(1))),selected='B1')),
   div(class='book-controls',selectInput('g1','第一組',survey$groups$zodiac$categories,selected='水瓶座'),selectInput('g2','第二組',survey$groups$zodiac$categories,selected='雙魚座')),
   div(class='book-controls',selectInput('sm','檢定方法',methods[1:3]),numericInput('sa','顯著水準 α',.05,min=.001,max=.2,step=.005),selectInput('st','檢定方向',tails),numericInput('sd0','假設差異 Δ₀',0)),
   conditionalPanel("input.sm == 'z'",div(class='book-controls',numericInput('sv1','第一組已知母體變異數 σ₁²',NA,min=0),numericInput('sv2','第二組已知母體變異數 σ₂²',NA,min=0)),hint('請由題目、外部資料或研究設定給定母體變異數。此處不會把樣本 s² 自動當成已知 σ²；這兩個輸入只用於目前所選變數。')),
   uiOutput('survey_info'),tableOutput('survey_summary'),uiOutput('survey_ci'),uiOutput('survey_result'),
   conditionalPanel("input.sm != 'z'",tags$details(tags$summary('檢視其餘變數與多重比較'),p('此表固定檢定 Δ₀=0；方向、α 與方法沿用上方設定。Holm 校正將這 13 個變數視為同一檢定家族。'),tableOutput('all_variables'),actionButton('export_results','下載比較結果 CSV',class='btn-primary'))),
   conditionalPanel("input.sm == 'z'",hint('各變數的已知母體變異數可能不同，因此 Z 檢定逐一分析目前變數，不將同一組 σ² 套用到所有變數。')),
   tags$details(tags$summary('資料處理與解讀限制'),includeHTML('www/data-notes.html')))),
  tabPanel('習作詳解',card(h3('課後習作：先作答，再展開解答'),p('以課本題意與完整精度重算。預設 α=.05，方向由題目決定。'),
   uiOutput('exercises'),actionButton('answers_download','下載習作詳解 HTML',class='btn-primary'))),
  tabPanel('自行輸入',card(h3('調整摘要數據，觀察檢定圖'),selectInput('mm','檢定方式',methods),
   div(class='book-controls',numericInput('n1','第一組 n₁',8,min=2,step=1),numericInput('m1','第一組平均數',3),numericInput('s1','第一組樣本 s₁',sqrt(16/7),min=0)),
   div(class='book-controls',numericInput('n2','第二組 n₂',8,min=2,step=1),numericInput('m2','第二組平均數',4.25),numericInput('s2','第二組樣本 s₂',sqrt(7.5/7),min=0)),
   conditionalPanel("input.mm == 'z'",div(class='book-controls',numericInput('variance1','第一組已知母體變異數 σ₁²',2,min=0),numericInput('variance2','第二組已知母體變異數 σ₂²',2,min=0)),hint('直接輸入 σ²，不需要先開根號。此檢定使用已知 σ²，與上方樣本標準差 s₁、s₂ 無關。')),
   conditionalPanel("input.mm == 'paired'",numericInput('rho','配對相關 r（由同一配對資料估計）',.912870929,min=-1,max=1,step=.05)),
   div(class='book-controls',numericInput('ma','顯著水準 α',.05,min=.001,max=.2,step=.005),selectInput('mt','檢定方向',tails),numericInput('md','虛無差異 Δ₀',0)),uiOutput('manual_result'))),
  tabPanel('下載與來源',includeHTML('www/sources.html'))))

server<-function(input,output,session){
 observeEvent(input$group,{cats<-unlist(survey$groups[[input$group]]$categories);updateSelectInput(session,'g1',choices=cats,selected=cats[1]);updateSelectInput(session,'g2',choices=cats,selected=cats[2])})
 observeEvent(input$example,{updateSelectInput(session,'bm',selected='welch');val<-if(input$example=='age')4 else 2;updateNumericInput(session,'bs1',value=val);updateNumericInput(session,'bs2',value=val)})
 current_book<-reactive({key<-if(input$example=='age')'age' else 'uber';book[[key]]})
 output$book_question<-renderUI({p(switch(input$example,uber='市區與郊區各 8 位不同上班族的每週 Uber 次數。第一組為市區，第二組為郊區。Z 示範假定兩組母體變異數皆為 2（σ=√2）。',age='管理研究生與心理研究生各 25 筆年齡。原始欄位並列不代表配對，這裡採獨立樣本。Z 示範假定兩組 σ=2。',paired='相同八位上班族分別在市區與郊區工作時的搭車次數，依原始資料逐列配對；差異為市區 − 郊區。',levene='以表 10.7 的兩組 Uber 次數檢驗變異數。Levene 使用平均數，Brown–Forsythe 使用中位數。'))})
 output$book_raw<-renderTable({x<-current_book();data.frame(序=x$a*0+seq_along(x$a),第一組=x$a,第二組=x$b)},digits=4)
 output$book_links<-renderUI({n<-if(input$example=='age')'Ch10p245age.xlsx' else if(input$example=='levene')'Ch10p266t10.7.xlsx' else 'Ch10p248ex10.1-10.4.xlsx';a(href=paste0('downloads/',n),download=NA,'下載對應 Excel 原檔')})
 output$book_result<-renderUI({safe_ui({x<-current_book();labels<-if(input$example=='age')c('管理研究生','心理研究生') else c('市區','郊區');
  if(input$example=='levene'){
   l<-levene_raw(x$a,x$b);bf<-levene_raw(x$a,x$b,TRUE)
   tagList(div(class='hypothesis','H₀：σ₁² = σ₂²；H₁：σ₁² ≠ σ₂²'),table_html(data.frame(方法=c('Levene（平均數）','Brown–Forsythe（中位數）'),W=fmt(c(l$W,bf$W)),df1=1,df2=l$df2,p值=pfmt(c(l$p,bf$p)),決策=ifelse(c(l$p,bf$p)<input$ba,'拒絕變異數相等','未能拒絕變異數相等'))),p('W 服從右尾 F 參考分配。未顯著不代表已證明變異數相同；不宜只憑前置檢定機械選擇 t 檢定，Welch 可直接作為不假設等變異數的方法。'))
  }else{
   method<-if(input$example=='paired')'paired' else input$bm
   r<-test_raw(x$a,x$b,method=method,delta0=input$bd,alpha=input$ba,tail=input$bt,sigma1=if(method=='z')variance_sigma(input$bs1) else NA,sigma2=if(method=='z')variance_sigma(input$bs2) else NA)
   comparison<-if(input$example!='paired')safe_ui({table_html(result_frame(lapply(c('z','pooled','welch'),function(m)test_raw(x$a,x$b,method=m,alpha=input$ba,tail=input$bt,delta0=input$bd,sigma1=if(m=='z')variance_sigma(input$bs1) else NA,sigma2=if(m=='z')variance_sigma(input$bs2) else NA))))}) else NULL
   tagList(show_result(r,labels),if(method=='welch')hint('Welch 自由度保留小數。Excel 原檔將自由度取整或先四捨五入統計量時，p 值會略有不同；本介面不為了對齊快取值而降低精度。'),if(input$example!='paired')tags$details(tags$summary('同一資料的三種獨立檢定比較'),comparison))
  }
 })})
 current_groups<-reactive({req(input$g1,input$g2,input$variable);validate(need(input$g1!=input$g2,'請選擇兩個不同群組。'));v<-survey$variables[[input$variable]];list(a=v$groups[[input$group]][[input$g1]],b=v$groups[[input$group]][[input$g2]],v=v)})
 output$survey_info<-renderUI({g<-current_groups();req(g$a,g$b);tagList(hint(paste('變數：',g$v$label,'；單位：',g$v$unit)),p('比較方向：',input$g1,' − ',input$g2,'。缺失值依目前分組與變數逐項排除，0 保留。'))})
 output$survey_summary<-renderTable({g<-current_groups();req(g$a,g$b)
  sigma<-function(v)if(input$sm=='z')tryCatch(variance_sigma(v),error=function(e)NA_real_) else NA_real_
  ci<-rbind(mean_ci(g$a,input$sa,if(input$sm=='z')'z' else 't',sigma(input$sv1)),mean_ci(g$b,input$sa,if(input$sm=='z')'z' else 't',sigma(input$sv2)))
  out<-data.frame(組別=c(input$g1,input$g2),分組人數=c(g$a$total,g$b$total),有效n=c(g$a$n,g$b$n),缺失=c(g$a$missing,g$b$missing),平均數=c(g$a$mean,g$b$mean),標準差=c(g$a$sd,g$b$sd),中位數=c(g$a$median,g$b$median))
  level<-paste0(format((1-input$sa)*100,trim=TRUE),'% CI ')
  out[[paste0(level,'LL')]]<-ci[,'LL'];out[[paste0(level,'UL')]]<-ci[,'UL'];out
 },digits=4)
 survey_test<-reactive({g<-current_groups();req(g$a,g$b);test_summary(g$a,g$b,method=input$sm,alpha=input$sa,tail=input$st,delta0=input$sd0,sigma1=if(input$sm=='z')variance_sigma(input$sv1) else NA,sigma2=if(input$sm=='z')variance_sigma(input$sv2) else NA)})
 output$survey_ci<-renderUI({safe_ui({g<-current_groups();req(g$a,g$b);if(min(g$a$n,g$b$n)<2)stop('每組需至少兩筆有效資料。');HTML(mean_ci_svg(g$a,g$b,c(input$g1,input$g2),input$sa,method=if(input$sm=='z')'z' else 't',sigma1=if(input$sm=='z')variance_sigma(input$sv1) else NA,sigma2=if(input$sm=='z')variance_sigma(input$sv2) else NA))})})
 output$survey_result<-renderUI({safe_ui({
  r<-survey_test();g<-current_groups();lev<-levene_summary(g$a,g$b);bf<-levene_summary(g$a,g$b,'medianAbs')
  lev_table<-data.frame(方法=c('Levene','Brown–Forsythe'),W=fmt(c(lev$W,bf$W)),p值=pfmt(c(lev$p,bf$p)))
  tagList(show_result(r,c(input$g1,input$g2)),tags$details(tags$summary('變異數同質性參考'),table_html(lev_table)),div(class='warning','星座、性別、手機與方案不是隨機分派。結果是群組關聯，不是因果證據。偏態、離群值及小組樣本數仍會影響推論。'))
 })})
 all_results<-reactive({req(input$g1!=input$g2);keys<-names(survey$variables);out<-lapply(keys,function(k){v<-survey$variables[[k]];a<-v$groups[[input$group]][[input$g1]];b<-v$groups[[input$group]][[input$g2]];if(is.null(a)||is.null(b))return(NULL);tryCatch({r<-test_summary(a,b,method=input$sm,alpha=input$sa,tail=input$st);data.frame(變數=v$label,n1=a$n,n2=b$n,差異=r$difference,t=r$stat,df=r$df,p=r$p)},error=function(e)data.frame(變數=v$label,n1=a$n,n2=b$n,差異=NA,t=NA,df=NA,p=NA))});a<-do.call(rbind,out);a$Holm_p<-p.adjust(a$p,'holm',n=length(keys));a})
 output$all_variables<-renderTable({a<-all_results();a$p<-pfmt(a$p);a$Holm_p<-pfmt(a$Holm_p);a},digits=4)
 observeEvent(input$export_results,{txt<-paste(capture.output(write.csv(all_results(),row.names=FALSE,na='')),collapse='\n');session$sendCustomMessage('download-file',list(content=paste0('\ufeff',txt),name='平均數差異比較.csv',mime='text/csv;charset=utf-8'))})
 output$manual_result<-renderUI({safe_ui({validate(need(input$n1==floor(input$n1)&&input$n2==floor(input$n2),'樣本數必須是整數。'));r<-test_summary(list(n=input$n1,mean=input$m1,sd=input$s1),list(n=input$n2,mean=input$m2,sd=input$s2),method=input$mm,alpha=input$ma,tail=input$mt,delta0=input$md,sigma1=if(input$mm=='z')variance_sigma(input$variance1) else NA,sigma2=if(input$mm=='z')variance_sigma(input$variance2) else NA,r=input$rho);show_result(r)})})
 exercises_ui<-function(){
  a<-list(n=36,mean=135,sd=16);b<-list(n=36,mean=127,sd=16)
  i<-test_summary(a,b,'pooled',tail='right');p<-test_summary(a,b,'paired',r=.8,tail='right');z<-test_summary(a,b,'z',sigma1=16,sigma2=16,tail='right')
  ta<-list(n=25,mean=10,sd=5);tb<-list(n=25,mean=0,sd=20);t1<-test_summary(ta,tb,'pooled',tail='right');t2<-test_summary(ta,tb,'welch',tail='right')
  x<-book$papers$a;y<-book$papers$b
  p1<-test_raw(c(x,y),method='one',delta0=4.5);p2<-test_raw(x,method='one',delta0=4.5);p3<-test_raw(y,method='one',delta0=4.5);p4<-test_raw(x,y,method='pooled');p5<-test_raw(x,y,method='welch');p6<-test_raw(x,y,method='paired')
  tagList(
   tags$details(tags$summary('習作一：世代智力差異與父子配對'),p('兩世代各 36 名，平均數 135 與 127，樣本標準差均 16；檢定新世代是否較高，採右尾 α=.05。'),
    tags$ol(tags$li('獨立設計：SE = √(16²/36＋16²/36) = 3.7712。樣本標準差估計母體時，使用 t，df=70。'),tags$li(sprintf('t=%s，右尾 p=%s，拒絕 H₀。',fmt(i$stat),pfmt(i$p))),tags$li(sprintf('36 對父子，r=.8：SE=√[(16²＋16²−2×.8×16×16)/36]=%s，df=35。',fmt(p$se))),tags$li(sprintf('配對 t=%s，右尾 p=%s，拒絕 H₀。',fmt(p$stat),pfmt(p$p))),tags$li('正相關配對可降低差異的標準誤，必須依實際抽樣設計選擇方法，不能為了顯著而改設計。')),
    table_html(result_frame(list(i,p,z))),hint('表中 Z 是額外的大樣本／已知 σ 教學對照：只有將 16 視為已知母體 σ，Z 才是已知變異數的檢定；題目若僅給樣本 s，主要答案採 t。'),diagrams(i)),
   tags$details(tags$summary('習作二：補習與成績，等變異數或 Welch？'),p('有／無補習各 25 人，平均數相差 10 分，樣本標準差分別為 5 與 20。問題「是否提高」採右尾 α=.05。'),
    tags$ol(tags$li('第一組 − 第二組 = 10。為計算差異，可把第二組平均數平移為 0；這不宣稱實際平均分數為 0。'),tags$li(sprintf('等變異數：sₚ²=(24×25＋24×400)/48=212.5；SE=√17=%s；df=48。',fmt(t1$se))),tags$li(sprintf('t=%s，右尾 p=%s，拒絕 H₀。',fmt(t1$stat),pfmt(t1$p))),tags$li(sprintf('Welch：SE 仍為 %s；df=%s；右尾 p=%s，拒絕 H₀。',fmt(t2$se),fmt(t2$df),pfmt(t2$p))),tags$li('兩組樣本數相同，因此這一題兩種 SE 相同；Welch 的自由度較小。觀察研究不能單靠差異檢定證明補習造成成績提高。')),table_html(result_frame(list(t1,t2))),diagrams(t2)),
   tags$details(tags$summary('習作三：女性學者論文篇數，單樣本、獨立或配對？'),p('無小孩：7,7,7,4,8,9,10,6；有小孩：3,7,8,0,4,6,8,2。各 8 名，全體平均 6 篇。各題使用雙尾 α=.05。'),
    tags$ol(tags$li(sprintf('全體 vs 4.5：n=16，M=6，s=%s；SE=%s；t(15)=%s，p=%s，拒絕 H₀。',fmt(p1$a$sd),fmt(p1$se),fmt(p1$stat),pfmt(p1$p))),tags$li(sprintf('無小孩 vs 4.5：t(7)=%s，p=%s，拒絕 H₀；有小孩 vs 4.5：t(7)=%s，p=%s，未能拒絕 H₀。',fmt(p2$stat),pfmt(p2$p),fmt(p3$stat),pfmt(p3$p))),tags$li(sprintf('兩獨立組：差異=2.5。等變異數 t(14)=%s，p=%s；Welch df=%s，p=%s，兩者皆未能拒絕 H₀。',fmt(p4$stat),pfmt(p4$p),fmt(p5$df),pfmt(p5$p))),tags$li(sprintf('如果為同八位學者前後測：r=%s，sD=2，SE=%s，t(7)=%s，p=%s，拒絕 H₀。',fmt(p6$r),fmt(p6$se),fmt(p6$stat),pfmt(p6$p))),tags$li('前兩題檢定各組母體平均數是否等於 4.5，第三題檢定兩獨立母體平均數差異，第四題檢定成對差異的平均數。配對資料依原列序，不得任意重排。')),
    table_html(cbind(檢定問題=c('全體 vs 4.5','無小孩 vs 4.5','有小孩 vs 4.5','無小孩 − 有小孩','無小孩 − 有小孩','同人前測 − 後測'),result_frame(list(p1,p2,p3,p4,p5,p6)))),hint('前三列估計值是各組平均數 M，CI 估計 μ；後三列估計值與 CI 是兩組／配對的平均差異。各題逐題 α=.05，並未把所有小題視為同一檢定家族。若研究中同時作多項推論，須另規劃多重比較。'),diagrams(p6),a(href='downloads/Ch10p274exercise10.3.xlsx',download=NA,'下載習作三 Excel 原檔')))
 }
 output$exercises<-renderUI(exercises_ui())
 observeEvent(input$answers_download,{content<-paste0('<!doctype html><html lang="zh-Hant"><meta charset="utf-8"><title>平均數差異檢定習作詳解</title><style>body{font:16px/1.8 Arial,sans-serif;max-width:1100px;margin:30px auto;padding:20px}table{border-collapse:collapse}td,th{padding:8px;border-bottom:1px solid #ddd}details{margin:20px 0}svg{max-width:550px}details>summary{font-weight:bold}</style><h1>平均數差異檢定習作詳解</h1>',as.character(exercises_ui()),'</html>');content<-gsub('<details>','<details open>',content,fixed=TRUE);session$sendCustomMessage('download-file',list(content=content,name='平均數差異檢定習作詳解.html',mime='text/html;charset=utf-8'))})
}
shinyApp(ui,server)


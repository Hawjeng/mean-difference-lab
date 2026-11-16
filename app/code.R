# Generate executable code for exactly the currently displayed test.
code_number<-function(x)format(x,digits=17,trim=TRUE,scientific=TRUE,decimal.mark='.')
code_string<-function(x)encodeString(x,quote='"')
code_vector<-function(x)paste(code_number(x),collapse=', ')
code_panel<-function(r_code,py_code,show_console=FALSE){
 block<-function(title,code,filename){
  editor<-div(
   tags$button(type='button',class='btn btn-default lab-code-download','data-filename'=filename,paste('下載',filename)),
   tags$textarea(class='form-control',rows=12,readonly=NA,spellcheck='false','aria-label'=paste(title,'語法'),code))
  output<-if(show_console){
   if(title=='R'){
    script<-textConnection(code,encoding='UTF-8')
    on.exit(close(script))
    transcript<-capture.output(source(script,local=new.env(),echo=TRUE,print.eval=TRUE,max.deparse.length=Inf))
    div(h5('R 主控台原始輸出'),tags$pre(class='syntax-console',paste(transcript,collapse='\n')))
   }else div(h5('Python 主控台原始輸出'),
    tags$button(type='button',class='btn btn-default lab-python-run','執行 Python'),
    tags$pre(class='syntax-console python-console',role='status','首次執行需下載 Python 與 SciPy。'))
  }
  div(class='syntax-block',tags$h4(title),if(show_console)div(class='syntax-columns',editor,output) else editor)
 }
 tags$details(class='lab-code',tags$summary('查看 R 與 Python 語法'),
  p('先列 R，再列 Python。R 使用內建函數；Python 需要 SciPy。可選取文字複製或下載。'),
  block('R',r_code,'mean-difference.R'),block('Python',py_code,'mean-difference.py'))
}
python_mean_body<-paste(readLines('www/mean-test-python.txt',encoding='UTF-8',warn=FALSE),collapse='\n')
mean_code<-function(r,show_console=FALSE){
 args<-list(n1=r$a$n,mean1=r$a$mean,sd1=r$a$sd)
 if(!is.null(r$b))args<-c(args,list(n2=r$b$n,mean2=r$b$mean,sd2=r$b$sd))
 for(key in c('delta0','alpha','sigma1','sigma2','r'))if(is.finite(r[[key]]))args[[key]]<-r[[key]]
 settings<-c(vapply(names(args),function(key)paste(key,'=',code_number(args[[key]])),character(1)),
             paste('method =',code_string(r$method)),paste('tail =',code_string(r$tail)))
 r_settings<-sub(' = ',' <- ',settings,fixed=TRUE)
 r_code<-paste(c('# 第一組減第二組；單樣本檢定使用第一組平均數。',
   r_settings,'',paste('test_summary <-',paste(deparse(test_summary),collapse='\n')),
   'a <- list(n=n1, mean=mean1, sd=sd1)',
   if(is.null(r$b))'b <- NULL' else 'b <- list(n=n2, mean=mean2, sd=sd2)',
   paste0('result <- test_summary(a, b, method=method, delta0=delta0, alpha=alpha, tail=tail',
       if(r$method=='z')', sigma1=sigma1, sigma2=sigma2',
       if(r$method=='paired')', r=r',')'),
   'print(result[c("difference","se","stat","df","p","critical","lower","upper","reject")])'),collapse='\n')
 py_code<-paste(c('# 第一組減第二組；單樣本檢定使用第一組平均數。',settings,'',python_mean_body),collapse='\n')
 code_panel(r_code,py_code,show_console)
}
variance_code<-function(x,y,alpha,show_console=FALSE){
 r_code<-paste(c(paste0('x <- c(',code_vector(x),')'),paste0('y <- c(',code_vector(y),')'),
  paste('alpha <-',code_number(alpha)),
  'for (center in list(mean, median)) {',
  '  z <- c(abs(x-center(x)), abs(y-center(y)))',
  '  group <- factor(rep(c("x", "y"), c(length(x),length(y))))',
  '  fit <- anova(lm(z ~ group))',
  '  print(fit)',
  '  print(fit[1,"Pr(>F)"] < alpha)',
  '}'),collapse='\n')
 py_code<-paste(c('# 安裝：python -m pip install scipy','from scipy import stats',
  paste0('x = [',code_vector(x),']'),paste0('y = [',code_vector(y),']'),
  paste('alpha =',code_number(alpha)),
  'for center in ("mean", "median"):',
  '    result = stats.levene(x, y, center=center)',
  '    print(center, result, "reject_H0 =", result.pvalue < alpha)'),collapse='\n')
 code_panel(r_code,py_code,show_console)
}

syntax_cards<-function(panel){
 tagList(lapply(panel$children[3:4],function(block){
  htmltools::tagAppendAttributes(block,class='lab-card')
 }))
}
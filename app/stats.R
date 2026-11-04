# Base-R statistical engine. Delta always means group 1 minus group 2.
fmt<-function(x,d=4)ifelse(is.finite(x),formatC(x,digits=d,format='f'),'—')
pfmt<-function(p)ifelse(p<.0001,'< .0001',fmt(p))
esc<-function(x){x<-gsub('&','&amp;',as.character(x),fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
variance_sigma<-function(v){if(length(v)!=1||!is.finite(v)||v<0)stop('請輸入有效的非負母體變異數 σ²；不是母體標準差 σ。');sqrt(v)}
summarize<-function(x){x<-x[is.finite(x)];list(n=length(x),mean=mean(x),sd=sd(x),median=median(x),q1=unname(quantile(x,.25)),q3=unname(quantile(x,.75)),min=min(x),max=max(x))}
test_summary<-function(a,b=NULL,method='welch',delta0=0,alpha=.05,tail='two',sigma1=NA,sigma2=NA,r=NA){
 stopifnot(is.finite(alpha),alpha>0,alpha<1,tail%in%c('two','left','right'),method%in%c('z','pooled','welch','paired','one'))
 if(a$n<2||!is.finite(a$sd)||a$sd<0)stop('第一組至少需要兩個有效觀察值與非負標準差。')
 if(method!='one'&&(is.null(b)||b$n<2||!is.finite(b$sd)||b$sd<0))stop('第二組至少需要兩個有效觀察值與非負標準差。')
 difference<-if(method=='one')a$mean else a$mean-b$mean
 df<-NA_real_;sp2<-NA_real_
 if(method=='z'){
  if(any(!is.finite(c(sigma1,sigma2)))||any(c(sigma1,sigma2)<0))stop('Z 檢定須給定已知母體標準差。')
  se<-sqrt(sigma1^2/a$n+sigma2^2/b$n)
 }else if(method=='one'){se<-a$sd/sqrt(a$n);df<-a$n-1
 }else if(method=='paired'){
  if(a$n!=b$n||!is.finite(r)||abs(r)>1)stop('配對設計需有相同配對數及 −1 至 1 的相關係數。')
  se<-sqrt(max(0,a$sd^2+b$sd^2-2*r*a$sd*b$sd)/a$n);df<-a$n-1
 }else if(method=='pooled'){
  df<-a$n+b$n-2;sp2<-((a$n-1)*a$sd^2+(b$n-1)*b$sd^2)/df;se<-sqrt(sp2*(1/a$n+1/b$n))
 }else{
  v1<-a$sd^2/a$n;v2<-b$sd^2/b$n;se<-sqrt(v1+v2);df<-(v1+v2)^2/(v1^2/(a$n-1)+v2^2/(b$n-1))
 }
 if(!is.finite(se)||se<=0)stop('標準誤為 0 或無法估計，不能套用一般 Z／t 檢定。')
 stat<-(difference-delta0)/se
 cdf<-if(method=='z')pnorm else function(x)pt(x,df)
 p<-if(tail=='two')2*cdf(-abs(stat)) else if(tail=='left')cdf(stat) else cdf(-stat)
 critical<-if(method=='z')qnorm(1-alpha/ifelse(tail=='two',2,1)) else qt(1-alpha/ifelse(tail=='two',2,1),df)
 ci_cv<-if(method=='z')qnorm(1-alpha/2) else qt(1-alpha/2,df)
 list(method=method,distribution=if(method=='z')'z' else 't',a=a,b=b,difference=difference,delta0=delta0,se=se,df=df,sp2=sp2,stat=stat,p=p,alpha=alpha,tail=tail,critical=critical,lower=difference-ci_cv*se,upper=difference+ci_cv*se,reject=p<alpha,r=r,sigma1=sigma1,sigma2=sigma2)
}
test_raw<-function(x,y=NULL,method='welch',...){
 if(method=='paired'){ok<-is.finite(x)&is.finite(y);x<-x[ok];y<-y[ok];r<-if(sd(x)>0&&sd(y)>0)cor(x,y) else 0;test_summary(summarize(x),summarize(y),method=method,r=r,...)}else test_summary(summarize(x),if(!is.null(y))summarize(y),method=method,...)
}
levene_summary<-function(a,b,center='meanAbs'){
 z1<-a[[center]];z2<-b[[center]];grand<-(a$n*z1$mean+b$n*z2$mean)/(a$n+b$n)
 ssb<-a$n*(z1$mean-grand)^2+b$n*(z2$mean-grand)^2;ssw<-(a$n-1)*z1$sd^2+(b$n-1)*z2$sd^2
 if(ssw<=0)return(list(W=NA,p=NA,df1=1,df2=a$n+b$n-2))
 W<-ssb/(ssw/(a$n+b$n-2));list(W=W,p=pf(W,1,a$n+b$n-2,lower.tail=FALSE),df1=1,df2=a$n+b$n-2)
}
levene_raw<-function(x,y,median_center=FALSE){f<-if(median_center)median else mean;a<-summarize(x);b<-summarize(y);a$meanAbs<-summarize(abs(x-f(x)));b$meanAbs<-summarize(abs(y-f(y)));levene_summary(a,b)}
method_name<-function(m)switch(m,z='獨立 Z：母體變異數已知',pooled='Student t：等變異數',welch='Welch t：不假設等變異數',paired='配對 t',one='單樣本 t')
hypothesis<-function(r,alternative=FALSE){op<-if(r$tail=='two')if(alternative)'≠' else '=' else if(r$tail=='left')if(alternative)'<' else '≥' else if(alternative)'>' else '≤';paste0(if(alternative)'H₁：' else 'H₀：',if(r$method=='one')'μ' else 'μ₁ − μ₂', ' ',op,' ',fmt(r$delta0))}
result_frame<-function(results){
 do.call(rbind,lapply(results,function(r)data.frame(方法=method_name(r$method),估計值=fmt(r$difference),SE=fmt(r$se),檢定量=fmt(r$stat),df=if(is.finite(r$df))fmt(r$df,3) else '—',p值=pfmt(r$p),雙側CI下限=fmt(r$lower),雙側CI上限=fmt(r$upper),決策=if(r$reject)'拒絕 H₀' else '未能拒絕 H₀',check.names=FALSE)))
}
svg_text<-function(x,y,label,color='#183d47',size=13,anchor='middle')sprintf('<text x="%.2f" y="%.2f" fill="%s" font-size="%s" text-anchor="%s">%s</text>',x,y,color,size,anchor,esc(label))
distribution_svg<-function(r,pview=FALSE){
 # Exact tail probabilities above; the grid is used only to draw areas.
 pdf<-if(r$distribution=='z')dnorm else function(x)dt(x,r$df)
 hi<-max(4.4,abs(r$stat)+.65,r$critical+.6);xx<-sort(unique(c(seq(-hi,hi,length.out=1401),r$stat,-abs(r$stat),abs(r$stat),-r$critical,r$critical)));dens<-pdf(xx)
 X<-function(x)52+466*(x+hi)/(2*hi);Y<-function(y)224-140*y/max(dens)
 mask<-if(pview){if(r$tail=='two')abs(xx)>=abs(r$stat) else if(r$tail=='left')xx<=r$stat else xx>=r$stat}else{if(r$tail=='two')abs(xx)>=r$critical else if(r$tail=='left')xx<= -r$critical else xx>=r$critical}
 color<-if(pview)'#087cb2' else '#c16a16'
 out<-c(sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 570 360" role="img" aria-label="%s" style="width:100%%;font-family:Microsoft JhengHei,Arial,sans-serif"><title>%s</title><rect width="570" height="360" fill="white"/>',esc(if(pview)'藍色 p-value 尾機率圖' else '橙色 alpha 拒絕域圖'),esc(paste(hypothesis(r),if(pview)paste('p =',pfmt(r$p)) else paste('alpha =',r$alpha)))),svg_text(285,24,if(pview)'p-value：觀察值及更極端的尾機率' else 'α：事先設定的拒絕域',color,16),svg_text(285,49,hypothesis(r)),svg_text(285,70,paste0(r$distribution,' 分配',if(is.finite(r$df))paste0('，df = ',fmt(r$df,3)) else '')))
 run<-rle(mask);ends<-cumsum(run$lengths);starts<-ends-run$lengths+1
 for(i in which(run$values)){ids<-starts[i]:ends[i];out<-c(out,sprintf('<polygon points="%.2f,224 %s %.2f,224" fill="%s" fill-opacity=".42"/>',X(xx[ids[1]]),paste(sprintf('%.2f,%.2f',X(xx[ids]),Y(dens[ids])),collapse=' '),X(xx[ids[length(ids)]]),color))}
 out<-c(out,sprintf('<polyline points="%s" fill="none" stroke="#344b58" stroke-width="2"/><path d="M52 224 H518" stroke="#344b58"/>',paste(sprintf('%.2f,%.2f',X(xx),Y(dens)),collapse=' ')))
 for(tick in pretty(c(-hi,hi),n=6))if(tick>=-hi&&tick<=hi)out<-c(out,svg_text(X(tick),246,format(tick,trim=TRUE),'#536c76',12))
 cvs<-if(r$tail=='two')c(-r$critical,r$critical) else if(r$tail=='left')-r$critical else r$critical
 for(cv in cvs)out<-c(out,sprintf('<path d="M%.2f 92 V224" stroke="#c16a16" stroke-width="1.6" stroke-dasharray="5 4"/>',X(cv)))
 out<-c(out,sprintf('<path d="M%.2f 88 V224" stroke="#7738b5" stroke-width="2.2"/>',X(r$stat)),svg_text(285,271,paste0(r$distribution,'-statistic（檢定量，非原始分數）')),
 svg_text(285,296,paste0('臨界值 = ',if(r$tail=='two')'±' else if(r$tail=='left')'−' else '',fmt(r$critical),'；α = ',r$alpha),'#c16a16'),svg_text(285,321,paste0(r$distribution,'_obt = ',fmt(r$stat),'；p = ',pfmt(r$p)),'#7738b5'),svg_text(285,345,if(pview)'藍色面積總和 = p-value' else paste0('橙色面積總和 = α',if(r$tail=='two')'；每尾 α/2' else ''),color),'</svg>');paste(out,collapse='')
}
mean_ci_svg<-function(a,b,labels=c('第一組','第二組'),alpha=.05,method='t',sigma1=NA,sigma2=NA){
 ss<-list(a,b);sigmas<-c(sigma1,sigma2);half<-vapply(1:2,function(i){s<-ss[[i]];if(method=='z')qnorm(1-alpha/2)*sigmas[i]/sqrt(s$n) else qt(1-alpha/2,s$n-1)*s$sd/sqrt(s$n)},numeric(1));means<-c(a$mean,b$mean);lo<-means-half;hi<-means+half;rg<-range(lo,hi);pad<-max(diff(rg)*.18,.01);rg<-rg+c(-pad,pad);X<-function(x)100+600*(x-rg[1])/diff(rg)
 out<-c('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 235" role="img" aria-label="兩組平均數與各組雙側信賴區間" style="width:100%;font-family:Microsoft JhengHei,Arial,sans-serif">',svg_text(400,25,paste0(fmt((1-alpha)*100,1),'% 各組平均數 ',method,' CI（非差異 CI）')))
 for(i in 1:2){s<-ss[[i]];y<-i*60;col<-if(i==1)'#347f90' else '#7738b5';out<-c(out,svg_text(90,y+4,labels[i],col,13,'end'),sprintf('<path d="M%.2f %s H%.2f M%.2f %s V%s M%.2f %s V%s" stroke="%s" stroke-width="3"/><circle cx="%.2f" cy="%s" r="5" fill="%s"/>',X(lo[i]),y,X(hi[i]),X(lo[i]),y-5,y+5,X(hi[i]),y-5,y+5,col,X(s$mean),y,col),svg_text(X(s$mean),y+25,paste0('M=',fmt(s$mean,2),'，n=',s$n),col,12))}
 for(tick in pretty(rg,n=6))if(tick>=rg[1]&&tick<=rg[2])out<-c(out,svg_text(X(tick),185,format(tick,trim=TRUE),'#536c76',12))
 paste(c(out,svg_text(400,218,'橫軸為所選變數原始單位；請用差異的 CI 與檢定作判斷。','#536c76',12),'</svg>'),collapse='')
}

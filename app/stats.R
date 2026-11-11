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
hypothesis<-function(r,alternative=FALSE){op<-if(r$tail=='two')if(alternative)'≠' else '=' else if(r$tail=='left')if(alternative)'<' else '≥' else if(alternative)'>' else '≤';paste0(if(alternative)'H₁：' else 'H₀：',if(r$method=='one')'μ' else 'μ₁ − μ₂', ' ',op,' ',format(r$delta0,trim=TRUE,scientific=FALSE))}
result_frame<-function(results){
 do.call(rbind,lapply(results,function(r)data.frame(方法=method_name(r$method),估計值=fmt(r$difference),SE=fmt(r$se),檢定量=fmt(r$stat),df=if(is.finite(r$df))fmt(r$df,3) else '—',p值=pfmt(r$p),雙側CI下限=fmt(r$lower),雙側CI上限=fmt(r$upper),決策=if(r$reject)'拒絕 H₀' else '未能拒絕 H₀',check.names=FALSE)))
}
svg_text<-function(x,y,label,color='#183d47',size=13,anchor='middle')sprintf('<text x="%.2f" y="%.2f" fill="%s" font-size="%s" text-anchor="%s">%s</text>',x,y,color,size,anchor,esc(label))
distribution_svg<-function(r){
 # Exact tail probabilities above; the grid is used only to draw areas.
 pdf<-if(r$distribution=='z')dnorm else function(x)dt(x,r$df)
 hi<-max(4.4,abs(r$stat)+.65,r$critical+.6);xx<-sort(unique(c(seq(-hi,hi,length.out=1401),r$stat,-abs(r$stat),abs(r$stat),-r$critical,r$critical)));dens<-pdf(xx)
 X<-function(x)62+636*(x+hi)/(2*hi);Y<-function(y)245-145*y/max(dens)
 alpha_mask<-if(r$tail=='two')abs(xx)>=r$critical else if(r$tail=='left')xx<= -r$critical else xx>=r$critical
 p_mask<-if(r$tail=='two')abs(xx)>=abs(r$stat) else if(r$tail=='left')xx<=r$stat else xx>=r$stat
 hatch_id<-paste0('alpha-hatch-',gsub('[^A-Za-z0-9]','',paste(r$distribution,r$tail,fmt(r$stat,8),fmt(r$critical,8),r$df)))
 out<-c(sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 910 393" role="img" aria-label="α 拒絕域與 p-value 尾機率整合圖" style="width:100%%;font-family:Microsoft JhengHei,Arial,sans-serif"><defs><pattern id="%s" patternUnits="userSpaceOnUse" width="6" height="6"><path d="M-1 1 L1 -1 M0 6 L6 0 M5 7 L7 5" stroke="#c16a16" stroke-width="1.5"/></pattern></defs><rect width="910" height="393" fill="white"/><g transform="translate(0,-25)">',hatch_id),svg_text(430,51,hypothesis(r)))
 shade<-function(mask,fill,opacity=1){
  run<-rle(mask);ends<-cumsum(run$lengths);starts<-ends-run$lengths+1;polygons<-character()
  for(i in which(run$values)){ids<-starts[i]:ends[i];polygons<-c(polygons,sprintf('<polygon points="%.2f,245 %s %.2f,245" fill="%s" fill-opacity="%s"/>',X(xx[ids[1]]),paste(sprintf('%.2f,%.2f',X(xx[ids]),Y(dens[ids])),collapse=' '),X(xx[ids[length(ids)]]),fill,opacity))}
  polygons
 }
 out<-c(out,shade(alpha_mask,'#ed9c38',.24),shade(p_mask,'#087cb2',.32),shade(alpha_mask,paste0('url(#',hatch_id,')')))
 out<-c(out,sprintf('<polyline points="%s" fill="none" stroke="#344b58" stroke-width="2"/><path d="M62 245 H698" stroke="#344b58"/>',paste(sprintf('%.2f,%.2f',X(xx),Y(dens)),collapse=' ')))
 ticks<-if(r$distribution=='z')seq(ceiling(-hi*2)/2,floor(hi*2)/2,by=.5) else pretty(c(-hi,hi),n=8)
 for(tick in ticks)if(tick>=-hi&&tick<=hi)out<-c(out,sprintf('<path class="axis-tick" data-value="%s" d="M%.2f 245 V250" stroke="#536c76"/>',tick,X(tick)),svg_text(X(tick),268,if(r$distribution=='z')fmt(tick,1) else format(tick,trim=TRUE),'#536c76',11))
 ci_half<-r$se*(if(r$distribution=='z')qnorm(.975) else qt(.975,r$df))
 ci_label<-paste0(if(r$method=='one')'平均數' else '平均數差異','的 95% CI（原始單位）：[',fmt(r$difference-ci_half),', ',fmt(r$difference+ci_half),']')
 cvs<-if(r$tail=='two')c(-r$critical,r$critical) else if(r$tail=='left')-r$critical else r$critical
 for(cv in cvs)out<-c(out,sprintf('<path d="M%.2f 95 V245" stroke="#c16a16" stroke-width="1.6" stroke-dasharray="5 4"/>',X(cv)))
 out<-c(out,sprintf('<path d="M%.2f 91 V245" stroke="#7738b5" stroke-width="2.2"/>',X(r$stat)),svg_text(710,250,if(r$distribution=='z')'z' else paste0('t(df=',fmt(r$df,2),' ≈ ',floor(r$df+.5),')'),'#183d47',12,'start'),
 svg_text(430,295,paste0('臨界值 = ',if(r$tail=='two')'±' else if(r$tail=='left')'−' else '',fmt(r$critical),'；',r$distribution,'_obt = ',fmt(r$stat)),'#7738b5'),
 svg_text(max(240,min(670,X(r$stat))),322,ci_label,'#7738b5',12),
 sprintf('<rect x="160" y="344" width="24" height="14" fill="#ed9c38" fill-opacity=".24"/><rect x="160" y="344" width="24" height="14" fill="url(#%s)" stroke="#c16a16"/>',hatch_id),svg_text(195,356,paste0('橙色斜線面積 = α = ',r$alpha,if(r$tail=='two')'（每尾 α/2）' else ''),'#c16a16',13,'start'),
 '<rect x="160" y="373" width="24" height="14" fill="#087cb2" fill-opacity=".32"/>',svg_text(195,385,paste0('藍色底色面積 = p-value = ',pfmt(r$p)),'#087cb2',13,'start'),'</g></svg>');paste(out,collapse='')
}
mean_ci<-function(s,alpha=.05,method='t',sigma=NA){
 if(s$n<2)return(c(LL=NA_real_,UL=NA_real_))
 half<-if(method=='z')qnorm(1-alpha/2)*sigma/sqrt(s$n) else qt(1-alpha/2,s$n-1)*s$sd/sqrt(s$n)
 c(LL=s$mean-half,UL=s$mean+half)
}
mean_ci_svg<-function(a,b,labels=c('第一組','第二組'),alpha=.05,method='t',sigma1=NA,sigma2=NA){
 ss<-list(a,b);sigmas<-c(sigma1,sigma2);ci<-vapply(1:2,function(i)mean_ci(ss[[i]],alpha,method,sigmas[i]),numeric(2));lo<-ci['LL',];hi<-ci['UL',];rg<-range(lo,hi);pad<-max(diff(rg)*.18,.01);rg<-rg+c(-pad,pad);X<-function(x)100+600*(x-rg[1])/diff(rg)
 out<-c('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 800 285" role="img" aria-label="兩組平均數與各組雙側信賴區間：LL 下限、UL 上限" style="width:100%;font-family:Microsoft JhengHei,Arial,sans-serif">',svg_text(400,25,paste0(fmt((1-alpha)*100,1),'% 各組平均數 ',method,' CI（非差異 CI）')))
 for(i in 1:2){s<-ss[[i]];y<-80+(i-1)*90;col<-if(i==1)'#347f90' else '#7738b5';out<-c(out,svg_text(90,y+4,labels[i],col,13,'end'),sprintf('<path d="M%.2f %s H%.2f M%.2f %s V%s M%.2f %s V%s" stroke="%s" stroke-width="3"/><circle cx="%.2f" cy="%s" r="5" fill="%s"/>',X(lo[i]),y,X(hi[i]),X(lo[i]),y-5,y+5,X(hi[i]),y-5,y+5,col,X(s$mean),y,col),svg_text(X(lo[i]),y-16,paste0('LL=',fmt(lo[i])),col,12,'end'),svg_text(X(hi[i]),y-16,paste0('UL=',fmt(hi[i])),col,12,'start'),svg_text(X(s$mean),y+25,paste0('M=',fmt(s$mean,2),'，n=',s$n),col,12))}
 for(tick in pretty(rg,n=6))if(tick>=rg[1]&&tick<=rg[2])out<-c(out,svg_text(X(tick),235,format(tick,trim=TRUE),'#536c76',12))
 paste(c(out,svg_text(400,268,'LL＝下限，UL＝上限；橫軸為原始單位，請用差異的 CI 與檢定作判斷。','#536c76',12),'</svg>'),collapse='')
}


source('app/stats.R',encoding='UTF-8');b<-jsonlite::fromJSON('app/data/textbook.json');s<-jsonlite::fromJSON('app/data/survey.json',simplifyVector=FALSE)
close<-function(x,y,tol=1e-9)stopifnot(isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tol)))
for(data in list(b$uber,b$age,b$papers))for(m in c('pooled','welch','paired'))for(tail in c('two','left','right')){
 r<-test_raw(data$a,data$b,m,tail=tail)
 ref<-t.test(data$a,data$b,paired=m=='paired',var.equal=m=='pooled',alternative=switch(tail,two='two.sided',left='less',right='greater'))
 close(r$stat,ref$statistic);close(r$df,ref$parameter);close(r$p,ref$p.value)
 if(tail=='two'){close(r$lower,ref$conf.int[1]);close(r$upper,ref$conf.int[2])}
}
for(v in list(b$papers$a,b$papers$b,c(b$papers$a,b$papers$b))){r<-test_raw(v,method='one',delta0=4.5);ref<-t.test(v,mu=4.5);close(r$p,ref$p.value);close(r$stat,ref$statistic)}
z<-test_raw(b$uber$a,b$uber$b,'z',sigma1=sqrt(2),sigma2=sqrt(2));close(z$stat,-1.76776695296637);close(z$p,.0770998717435418)
left<-test_raw(b$uber$a,b$uber$b,'z',tail='left',sigma1=sqrt(2),sigma2=sqrt(2));right<-test_raw(b$uber$a,b$uber$b,'z',tail='right',sigma1=sqrt(2),sigma2=sqrt(2));close(left$p+right$p,1);stopifnot(right$p>.5)
swapped<-test_raw(b$uber$b,b$uber$a,'welch');orig<-test_raw(b$uber$a,b$uber$b,'welch');close(orig$stat,-swapped$stat);close(orig$p,swapped$p)
nonzero<-test_raw(b$uber$a,b$uber$b,'welch',delta0=-1);close(nonzero$p,t.test(b$uber$a,b$uber$b,mu=-1)$p.value)
for(cen in c(FALSE,TRUE)){
 f<-if(cen)median else mean;z1<-abs(b$uber$a-f(b$uber$a));z2<-abs(b$uber$b-f(b$uber$b));ref<-summary(aov(c(z1,z2)~factor(rep(1:2,each=8))))[[1]]
 r<-levene_raw(b$uber$a,b$uber$b,cen);close(r$W,ref[1,'F value']);close(r$p,ref[1,'Pr(>F)'])
}
stopifnot(inherits(try(test_summary(list(n=1,mean=1,sd=0),list(n=2,mean=2,sd=1)),silent=TRUE),'try-error'))
stopifnot(inherits(try(test_summary(list(n=8,mean=1,sd=1),list(n=9,mean=2,sd=1),'paired',r=.5),silent=TRUE),'try-error'))
stopifnot(inherits(try(test_summary(list(n=8,mean=1,sd=0),list(n=8,mean=2,sd=0)),silent=TRUE),'try-error'))
for(v in s$variables)for(g in v$groups){for(pair in combn(names(g),2,simplify=FALSE))for(m in c('welch','pooled')){
 a<-g[[pair[1]]];c<-g[[pair[2]]];r<-test_summary(a,c,m);stopifnot(is.finite(r$p),r$p>=0,r$p<=1,r$df>0,r$lower<r$upper)
}}
cat('PASS: native R t.test equivalence, one-/two-sided tails, nonzero null, Levene, invalid inputs and all survey group pairs.\n')
vz<-test_raw(b$uber$a,b$uber$b,'z',sigma1=variance_sigma(4),sigma2=variance_sigma(9))
close(vz$se,sqrt(4/8+9/8));close(vz$stat,(-1.25)/sqrt(13/8))
close(variance_sigma(0),0)
for(bad in list(-1,NA_real_,numeric(0),Inf))stopifnot(inherits(try(variance_sigma(bad),silent=TRUE),'try-error'))
cat('PASS: known variance inputs, squared-unit conversion, unequal population variances, invalid entries.\n')

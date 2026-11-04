# Reproduce the textbook answers using base R.
# Run from the project root. Only jsonlite is required for the input file.
source('app/stats.R',encoding='UTF-8')
b<-jsonlite::fromJSON('app/data/textbook.json')
cat('\nUber: independent Z, Student, Welch, and paired t\n')
print(result_frame(lapply(c('z','pooled','welch','paired'),function(m)test_raw(b$uber$a,b$uber$b,m,sigma1=sqrt(2),sigma2=sqrt(2)))))
cat('\nLevene and Brown-Forsythe\n');print(levene_raw(b$uber$a,b$uber$b));print(levene_raw(b$uber$a,b$uber$b,TRUE))
cat('\nGraduate ages: independent designs\n')
print(result_frame(lapply(c('z','pooled','welch'),function(m)test_raw(b$age$a,b$age$b,m,sigma1=2,sigma2=2))))
cat('\nExercise 1: generation IQ, right-tailed\n')
a<-list(n=36,mean=135,sd=16);c<-list(n=36,mean=127,sd=16)
print(result_frame(list(test_summary(a,c,'pooled',tail='right'),test_summary(a,c,'paired',tail='right',r=.8))))
cat('\nExercise 2: tutoring, right-tailed\n')
a<-list(n=25,mean=10,sd=5);c<-list(n=25,mean=0,sd=20)
print(result_frame(lapply(c('pooled','welch'),function(m)test_summary(a,c,m,tail='right'))))
cat('\nExercise 3: papers, two-tailed\n');x<-b$papers$a;y<-b$papers$b
print(result_frame(c(lapply(list(c(x,y),x,y),function(v)test_raw(v,method='one',delta0=4.5)),lapply(c('pooled','welch','paired'),function(m)test_raw(x,y,m)))))
# Survey summaries: compare two chosen zodiac groups without individual records.
s<-jsonlite::fromJSON('app/data/survey.json',simplifyVector=FALSE)
a<-s$variables$B1$groups$zodiac[['水瓶座']];c<-s$variables$B1$groups$zodiac[['雙魚座']]
print(result_frame(list(test_summary(a,c,'welch'))))

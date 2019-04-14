dat<-read.csv("sleepcut.csv", header=T) 
head(dat)

m4<-lm(CognitionZscore~ NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
summary(m4)

m1<-lm(CognitionZscore ~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore, data = dat)
summary(m1)

null=lm(CognitionZscore~Gender + NumEarlyClass + EarlyClass + GPA + StressScore,data=dat)
full=lm(CognitionZscore~.,data=dat)
step(null, scope=list(lower=null, upper=full),direction="forward")
m1.05<-lm(formula = CognitionZscore ~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore + Stress, data = dat)
summary(m1.05)

m1.1<-lm(CognitionZscore ~ ClassesMissed+ AnxietyStatus+ Gender + NumEarlyClass + EarlyClass + GPA, data = dat)
summary(m1.1)
anova(m1,m1.1)

mforwards<-lm(CognitionZscore ~ GPA + Gender + StressScore + ClassesMissed + AnxietyStatus, data = dat)
summary(mforwards)

null=m4
full=lm(CognitionZscore~.,data=dat)
step(null, scope=list(lower=null, upper=full),direction="backward")

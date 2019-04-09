dat<-read.csv("sleepcut.csv", header=T) 
head(dat)

m4<-lm(CognitionZscore~ NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
summary(m4)

null=m4
full=lm(CognitionZscore~.,data=dat)
step(null, scope=list(lower=null, upper=full),direction="backward")

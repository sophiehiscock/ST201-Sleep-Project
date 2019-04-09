dat<-read.csv("sleep.csv", header=T) #missing first 10 rows so we can test with this later
dat1<-read.csv("fortest.csv", header=T)
head(dat)

m1<-lm(CognitionZscore ~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore, data = dat)
summary(m1)
m4<-lm(CognitionZscore~ NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
summary(m4)

predict(m1,list(Gender=0,NumEarlyClass=2,EarlyClass=1,GPA=4,StressScore=20))
predict(m1,newdata=dat1,interval='confidence')

predict(m4,newdata=dat1)
#interval='confidence',

install.packages("sigr")
d<-data.frame(prediction=test$CognitionZscore,actual=dat$CognitionZscore)
cor.test(d$prediction,d$actual)

rand <- sample(1:254, 20, replace = FALSE)
test <- dat[rand, ]
a <- predict(m4,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)

help(cor)

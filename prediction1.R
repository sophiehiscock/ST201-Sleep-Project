dat<-read.csv("sleep.csv", header=T) #missing first 10 rows so we can test with this later
dat1<-read.csv("sleepcut.csv", header=T)
head(dat)

m13<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality+ StressScore + Drinks + WeekdayBed + WeekdayRise + WeekdayRise*GPA+ NumEarlyClass*GPA, data=dat1)
summary(m13)
m4<-lm(CognitionZscore~ NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat1)
summary(m4)

rand <- sample(1:254, 25, replace = FALSE)
print(rand)
test <- dat[rand, ]
a <- predict(m13,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)

rand <- sample(1:254, 20, replace = FALSE)
test <- dat[rand, ]
print(rand)
a <- predict(m4,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)

rand <- sample(1:254, 25, replace = FALSE)
test <- dat[rand, ]
a <- predict(m1,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)
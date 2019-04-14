dat<-read.csv("sleep.csv", header=T) #missing first 10 rows so we can test with this later
dat1<-read.csv("sleepcut.csv", header=T)
dat2<- read.csv("fortest.csv", header=T)

#models to test_______________________________________

m13<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality+ StressScore + Drinks + WeekdayBed + WeekdayRise + WeekdayRise*GPA+ NumEarlyClass*GPA, data=dat1)
summary(m13)
m4<-lm(CognitionZscore~ NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender, dat1)
summary(m4)
m1<-lm(CognitionZscore ~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore, data = dat1)
summary(m1)


#this shows how well the model (trained on the cut dataset) predicts the values of the unused rows_____

#m4
b<-predict(m4,newdata=dat2,interval='confidence')
cor(dat2$CognitionZscore, b) #0.6320178

#m1
c<-predict(m1,newdata=dat2,interval='confidence')
cor(dat2$CognitionZscore, c) #0.4938448

#m13
d<-predict(m13,newdata=dat2,interval='confidence')
cor(dat2$CognitionZscore, d) #0.4460338

#random sampling__________________________________________________

rand <- sample(1:254, 25, replace = FALSE)
print(rand)
test <- dat[rand, ]
a <- predict(m13,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)

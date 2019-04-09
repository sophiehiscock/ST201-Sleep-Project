dat<-read.csv("sleep.csv", header=T) #missing first 10 rows so we can test with this later
head(dat)

m13<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality+ StressScore + Drinks + WeekdayBed + WeekdayRise + WeekdayRise*GPA+ NumEarlyClass*GPA, data=dat)
summary(m13)

rand <- sample(1:254, 25, replace = FALSE)
test <- dat[rand, ]
a <- predict(m13,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)

rand <- sample(1:254, 25, replace = FALSE)
test <- dat[rand, ]
a <- predict(m4,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)

rand <- sample(1:254, 25, replace = FALSE)
test <- dat[rand, ]
a <- predict(m1,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)
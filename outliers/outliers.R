#dat<-read.csv("sleepedit.csv",header=T) #pre-edited data, without aliased coefficients
dat<-read.csv("sleepcut.csv", header=T) #missing first 10 rows so we can test with this later
head(dat)

#testing for outliers in: DASScore
m1<-lm(CognitionZscore~ DASScore, data=dat) 
m1.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m1),2),Lev=round(hatvalues(m1),2),Cooks=round(cooks.distance(m1),2)))
write.csv(m1.outliers,row.names = FALSE,"DASScore.outliers.csv")

#NumEarlyClass
m2<-lm(CognitionZscore~ NumEarlyClass, data=dat) 
write.csv(m2.outliers,row.names = FALSE,"NumEarlyClass.outliers.csv")
m2.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m2),2),Lev=round(hatvalues(m2),2),Cooks=round(cooks.distance(m2),2)))

#GPA
m3<-lm(CognitionZscore~ GPA, data=dat) 
m3.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m3),2),Lev=round(hatvalues(m3),2),Cooks=round(cooks.distance(m3),2)))
write.csv(m3.outliers,row.names = FALSE,"GPA.outliers.csv")

#ClassesMissed
m4<-lm(CognitionZscore~ ClassesMissed, data=dat) 
m4.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m4),2),Lev=round(hatvalues(m4),2),Cooks=round(cooks.distance(m4),2)))
write.csv(m4.outliers,row.names = FALSE,"ClassesMissed.outliers.csv")

#CognitionZScore
m5<-lm(DASScore~CognitionZscore, data=dat) 
m5.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m5),2),Lev=round(hatvalues(m5),2),Cooks=round(cooks.distance(m5),2)))
write.csv(m5.outliers,row.names = FALSE,"CognitionZScore.outliers.csv")

#PoorSLeepQuality
m6<-lm(CognitionZscore~ PoorSleepQuality, data=dat) 
m6.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m6),2),Lev=round(hatvalues(m6),2),Cooks=round(cooks.distance(m6),2)))
write.csv(m6.outliers,row.names = FALSE,"PoorSleepQuality.outliers.csv")

#Happiness
m7<-lm(CognitionZscore~ Happiness, data=dat) 
m7.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m7),2),Lev=round(hatvalues(m7),2),Cooks=round(cooks.distance(m7),2)))
write.csv(m7.outliers,row.names = FALSE,"Happiness.outliers.csv")

#Drinks
m8<-lm(CognitionZscore~ Drinks, data=dat) 
m8.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m8),2),Lev=round(hatvalues(m8),2),Cooks=round(cooks.distance(m8),2)))
write.csv(m8.outliers,row.names = FALSE,"Drinks.outliers.csv")

#WeekdaySleep
m9<-lm(CognitionZscore~ WeekdaySleep, data=dat) 
m9.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m9),2),Lev=round(hatvalues(m9),2),Cooks=round(cooks.distance(m9),2)))
write.csv(m9.outliers,row.names = FALSE,"WeekdaySleep.outliers.csv")

#WeekendSleep
m10<-lm(CognitionZscore~ WeekendSleep, data=dat) 
m10.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m10),2),Lev=round(hatvalues(m10),2),Cooks=round(cooks.distance(m10),2)))
write.csv(m10.outliers,row.names = FALSE,"WeekendSleep.outliers.csv")

#AverageSleep
m11<-lm(CognitionZscore~ AverageSleep, data=dat) 
m11.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m11),2),Lev=round(hatvalues(m11),2),Cooks=round(cooks.distance(m11),2)))
write.csv(m11.outliers,row.names = FALSE,"AverageSleep.outliers.csv")

#DepressionScore
m12<-lm(CognitionZscore~ DepressionScore, data=dat) 
m12.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m12),2),Lev=round(hatvalues(m12),2),Cooks=round(cooks.distance(m12),2)))
write.csv(m12.outliers,row.names = FALSE,"DepressionScore.outliers.csv")

#AnxietyScore
m13<-lm(CognitionZscore~ AnxietyScore, data=dat) 
m13.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m13),2),Lev=round(hatvalues(m13),2),Cooks=round(cooks.distance(m13),2)))
write.csv(m13.outliers,row.names = FALSE,"AnxietyScore.outliers.csv")

#StressScore
m14<-lm(CognitionZscore~ StressScore, data=dat) 
m14.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m14),2),Lev=round(hatvalues(m14),2),Cooks=round(cooks.distance(m14),2)))
write.csv(m14.outliers,row.names = FALSE,"StressScore.outliers.csv")

#WeekdayBed
m15<-lm(CognitionZscore~ WeekdayBed, data=dat) 
m15.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m15),2),Lev=round(hatvalues(m15),2),Cooks=round(cooks.distance(m15),2)))
write.csv(m15.outliers,row.names = FALSE,"WeekdayBed.outliers.csv")

#WeekdayRise
m16<-lm(CognitionZscore~ WeekdayRise, data=dat) 
m16.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m16),2),Lev=round(hatvalues(m16),2),Cooks=round(cooks.distance(m16),2)))
write.csv(m16.outliers,row.names = FALSE,"WeekdayRise.outliers.csv")

#WeekendBed
m17<-lm(CognitionZscore~ WeekendBed, data=dat) 
m17.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m17),2),Lev=round(hatvalues(m17),2),Cooks=round(cooks.distance(m17),2)))
write.csv(m17.outliers,row.names = FALSE,"WeekendBed.outliers.csv")

#WeekendRise
m18<-lm(CognitionZscore~ WeekendRise, data=dat) 
m18.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m18),2),Lev=round(hatvalues(m18),2),Cooks=round(cooks.distance(m18),2)))
write.csv(m18.outliers,row.names = FALSE,"WeekendRise.outliers.csv")

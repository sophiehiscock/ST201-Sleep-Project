#dat<-read.csv("sleepedit.csv",header=T) #pre-edited data, without aliased coefficients
dat<-read.csv("sleepcut.csv", header=T) #missing first 10 rows so we can test with this later
head(dat)

#testing for outliers in DASScore
m1<-lm(CognitionZscore~ DASScore, data=dat) 
m1.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m1),2),Lev=round(hatvalues(m1),2),Cooks=round(cooks.distance(m1),2)))
write.csv(m1.outliers,row.names = FALSE,"DASScore.outliers.csv")

#NumEarlyClass
m2<-lm(CognitionZScore~ NumEarlyClass, data=dat) 
m2.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m2,row.names = FALSE,"NumEarlyClass.outliers.csv")

#GPA
m3<-lm(CognitionZscore~ GPA, data=dat) 
m3.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m3,row.names = FALSE,"GPA.outliers.csv")

#ClassesMissed
m4<-lm(CognitionZscore~ ClassesMissed, data=dat) 
m4.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m4,row.names = FALSE,"ClassesMissed.outliers.csv")

#CognitionZScore
m5<-lm(DASScore~CognitionZscore, data=dat) 
m5.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m5,row.names = FALSE,"CognitionZScore.outliers.csv")

#PoorSLeepQuality
m6<-lm(CognitionZscore~ PoorSLeepQuality, data=dat) 
m6.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m6,row.names = FALSE,"PoorSLeepQuality.outliers.csv")

#Happiness
m7<-lm(CognitionZscore~ Happiness, data=dat) 
m7.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m7,row.names = FALSE,"Happiness.outliers.csv")

#Drinks
m8<-lm(CognitionZscore~ Happiness, data=dat) 
m8.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m8,row.names = FALSE,"Happiness.outliers.csv")

#WeekdaySleep
m9<-lm(CognitionZscore~ WeekdaySleep, data=dat) 
m9.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m9,row.names = FALSE,"WeekdaySleep.outliers.csv")

#WeekendSleep
m10<-lm(CognitionZscore~ WeekendSleep, data=dat) 
m10.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m10,row.names = FALSE,"WeekendSleep.outliers.csv")

#AverageSleep
m11<-lm(CognitionZscore~ AverageSleep, data=dat) 
m11.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(m11,row.names = FALSE,"AverageSleep.outliers.csv")

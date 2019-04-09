---
  #Title: ST201 Project: Outlier Analysis
  #Author: Andrea Solis Olivares
---

  dat<-read.csv("sleep2.csv", header=T)
  head(dat)

#Testing for outliers 
  

  #testing for outliers in: DASScore
  m1<-lm(CognitionZscore~ DASScore, data=dat) 
  m1.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m1),2),Lev=round(hatvalues(m1),2),Cooks=round(cooks.distance(m1),2)))
  write.csv(m1.outliers,row.names = FALSE,"DASScore.outliers.csv")
  plot(m1)
  m1.outliers
  plot(m1.outliers)
  cooks.distance(m1)
  with(dat,plot(CognitionZscore,cooks.distance(m1)))
  
  identify(dat$CognitionZscore,cooks.distance(m1))
  lev
  lev=hatvalues(m1)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  #Observations with high leverage are 43, 72, 88, 117, 181, 182, 231
  #observation number with violation of cooks distance 181 , however it is less than 0.5 and thus has little influence on the regression


  
  plot(dat$CognitionZscore,rstandard(m1))
  
  plot(dat$CognitionZscore,lev)
  
  plot(m1)
  
  
  ----
    #note: can copy the above to generate graphs 
    
  ---
  
  #NumEarlyClass
  m2<-lm(CognitionZscore~ NumEarlyClass, data=dat) 
  write.csv(m2.outliers,row.names = FALSE,"NumEarlyClass.outliers.csv")
  m2.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m2),2),Lev=round(hatvalues(m2),2),Cooks=round(cooks.distance(m2),2)))
  
  plot(m2)
  par(mfrow=c(2,2))
  m2.outliers
  plot(m2.outliers)
  cooks.distance(m2)
  with(dat,plot(CognitionZScore,cooks.distance(m2)))
  
  identify(dat$CognitionZscore,cooks.distance(m2))
  lev
  lev=hatvalues(m2)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
  #GPA
  m3<-lm(CognitionZscore~ GPA, data=dat) 
  m3.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m3),2),Lev=round(hatvalues(m3),2),Cooks=round(cooks.distance(m3),2)))
  write.csv(m3.outliers,row.names = FALSE,"GPA.outliers.csv")
  
  plot(m3)
  par(mfrow=c(2,2))
  m3.outliers
  plot(m3.outliers)
  cooks.distance(m3)
  with(dat,plot(CognitionZScore,cooks.distance(m3)))
  
  identify(dat$CognitionZscore,cooks.distance(m3))
  lev
  lev=hatvalues(m3)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
  #Highly leveraged observations 250, 251, 252, 253
  
  #ClassesMissed
  m4<-lm(CognitionZscore~ ClassesMissed, data=dat) 
  m4.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m4),2),Lev=round(hatvalues(m4),2),Cooks=round(cooks.distance(m4),2)))
  write.csv(m4.outliers,row.names = FALSE,"ClassesMissed.outliers.csv")
  
  plot(m4)
  
  lev
  lev=hatvalues(m4)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
  cooks.distance(m4)
  with(dat,plot(CognitionZscore,cooks.distance(m4)))
  identify(dat$CognitionZscore,cooks.distance(m4)) 
  
  #No observations violate Cooks Distance
  
  #CognitionZScore
  m5<-lm(DASScore~CognitionZscore, data=dat) 
  m5.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m5),2),Lev=round(hatvalues(m5),2),Cooks=round(cooks.distance(m5),2)))
  write.csv(m5.outliers,row.names = FALSE,"CognitionZScore.outliers.csv")
  
  plot(m5)
  
  lev
  lev=hatvalues(m5)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
  cooks.distance(m5)
  with(dat,plot(CognitionZscore,cooks.distance(m5)))
  identify(dat$CognitionZscore,cooks.distance(m5)) 
  
  #Highly leveraged observations = 13 93 102 132 133 188
  #No observations violate Cooks Distance
  
  #PoorSLeepQuality
  m6<-lm(CognitionZscore~ PoorSleepQuality, data=dat) 
  m6.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m6),2),Lev=round(hatvalues(m6),2),Cooks=round(cooks.distance(m6),2)))
  write.csv(m6.outliers,row.names = FALSE,"PoorSleepQuality.outliers.csv")
  
  plot(m6)
  par(mfrow=c(2,2))
  m6.outliers
  plot(m6.outliers)
  cooks.distance(m6)
  with(dat,plot(PoorSleepQuality,cooks.distance(m6)))
  
  identify(dat$PoorSleepQuality,cooks.distance(m6))
  lev
  lev=hatvalues(m6)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
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
  
  #WeekdayBed
  m12<-lm(CognitionZscore~ WeekdayBed, data=dat) 
  m12.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m12),2),Lev=round(hatvalues(m12),2),Cooks=round(cooks.distance(m12),2)))
  write.csv(m12.outliers,row.names = FALSE,"WeekdayBed.outliers.csv")
  
  plot(m12)
  par(mfrow=c(2,2))
  m12.outliers
  plot(m12.outliers)
  cooks.distance(m12)
  with(dat,plot(WeekdayBed,cooks.distance(m12)))
  
  identify(dat$WeekdayBed,cooks.distance(m12))
  lev
  lev=hatvalues(m12)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
#Highly leveraged observations are 55, 164, 200, 203, 237, 246
#No observations fall outside of Cook's Distance
  
  
  #WeekdayRise
  m13<-lm(CognitionZscore~ WeekdayRise, data=dat) 
  m13.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m13),2),Lev=round(hatvalues(m13),2),Cooks=round(cooks.distance(m13),2)))
  write.csv(m13.outliers,row.names = FALSE,"WeekdayRise.outliers.csv")
  plot(m13)
  
  lev
  lev=hatvalues(m13)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
  
#No observations fall outside of Cook's Distance
#Highly leveraged values are 53, 95, 172, 195, 200, 229, 246
  
  
  #WeekendBed
  m14<-lm(CognitionZscore~ WeekendBed, data=dat) 
  m14.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m14),2),Lev=round(hatvalues(m14),2),Cooks=round(cooks.distance(m14),2)))
  write.csv(m14.outliers,row.names = FALSE,"WeekendBed.outliers.csv")
  plot(m14)
  
  lev
  lev=hatvalues(m14)
  lev[lev>2*2/253]
  lev[lev>3*2/253]

  cooks.distance(m14)
  with(dat,plot(CognitionZscore,cooks.distance(m14)))
  identify(dat$CognitionZscore,cooks.distance(m14))
  
  #No observations fall outside of Cook's Distance
  #Highly leveraged values are 54, 61, 86, 107, 200, 237, 246

  #WeekendRise
  m15<-lm(CognitionZscore~ WeekendRise, data=dat) 
  m15.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m15),2),Lev=round(hatvalues(m15),2),Cooks=round(cooks.distance(m15),2)))
  write.csv(m15.outliers,row.names = FALSE,"WeekendRise.outliers.csv")
  plot(m15)
  
  lev
  lev=hatvalues(m15)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
  cooks.distance(m15)
  with(dat,plot(CognitionZscore,cooks.distance(m15)))
  identify(dat$CognitionZscore,cooks.distance(m15))
  
  #Outliers exist, but they do not have much influence on the regression 
  #Highly leveraged values 54, 98, 108, 155, 156, 200, 243, 250
  #No observations fall outside of Cook's distance, meaning outliers will not have influence on linear regression 
  
  #DepressionScore
  m16<-lm(CognitionZscore~ DepressionScore, data=dat) 
  m16.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m16),2),Lev=round(hatvalues(m16),2),Cooks=round(cooks.distance(m16),2)))
  write.csv(m16.outliers,row.names = FALSE,"DepressionScore.outliers.csv")
  plot(m16)
  
  lev
  lev=hatvalues(m16)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
  cooks.distance(m16)
  with(dat,plot(CognitionZscore,cooks.distance(m16)))
  identify(dat$CognitionZscore,cooks.distance(m16)) 
  
  #Highly leveraged observations are 25, 43, 51, 88, 117, 151, 172, 177, 181, 182, 231, 239
  #No observations violate Cooks Distance
  
  #Anxiety Score
  m17<-lm(CognitionZscore~ AnxietyScore, data=dat) 
  m17.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m17),2),Lev=round(hatvalues(m17),2),Cooks=round(cooks.distance(m17),2)))
  write.csv(m17.outliers,row.names = FALSE,"AnxietyScore.outliers.csv")
  plot(m17)
  
  lev
  lev=hatvalues(m17)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
  cooks.distance(m17)
  with(dat,plot(CognitionZscore,cooks.distance(m17)))
  identify(dat$CognitionZscore,cooks.distance(m17)) 
  
  #Highly leveraged observations are 43, 48, 74, 126, 143, 164, 181, 182, 203, 231
  #No observations violate Cooks Distance
  
  #StressScore
  m18<-lm(CognitionZscore~ DepressionScore, data=dat) 
  m18.outliers<-data.frame(Stand.Res=cbind(round(rstandard(m18),2),Lev=round(hatvalues(m18),2),Cooks=round(cooks.distance(m18),2)))
  write.csv(m18.outliers,row.names = FALSE,"DepressionScore.outliers.csv")
  plot(m18)
  
  lev
  lev=hatvalues(m18)
  lev[lev>2*2/253]
  lev[lev>3*2/253]
  
  cooks.distance(m18)
  with(dat,plot(CognitionZscore,cooks.distance(m18)))
  identify(dat$CognitionZscore,cooks.distance(m18)) 
  
  #Highly leveraged observations are 25 43 51 88 117 151 172 177 181 182 231 239
  #No observations violate Cooks Distance
  
 
  
#dat<-read.csv("sleepedit.csv",header=T) #pre-edited data, without aliased coefficients
dat<-read.csv("sleep.csv", header=T)
head(dat)

#turning the categorical variables into numbers within the code:
dat$LarkOwl<-relevel(dat$LarkOwl,ref="Neither")
dat$DepressionStatus<-relevel(dat$DepressionStatus, ref="moderate")
dat$AnxietyStatus<-relevel(dat$AnxietyStatus, ref="severe")
dat$Stress<-relevel(dat$Stress, ref="high")
dat$AlcoholUse<-relevel(dat$AlcoholUse, ref="Light")

#linear regression on all variables
m.a<-lm(CognitionZscore~., data=dat)
summary(m.a)
#can see that DASScore is perfectly correlated with other variables in the model

#all variables except DASScore
m.b<-lm(CognitionZscore~.-DASScore, data=dat)
summary(m.b)
cor(dat$NumEarlyClass,dat$ClassesMissed) #negative correlation?????
m<-lm(CognitionZscore~ ClassesMissed+NumEarlyClass, data=dat)
vif(m) #no collinearity

install.packages('car')
library(car)
vif(m.b)
#VIF of 1 means not correlated, VIF of more than 10 highly correlated

#Model with one variable from each "group"
m.c<-lm(CognitionZscore~ Gender+ClassYear+ClassesMissed+AverageSleep+DASScore+AlcoholUse+GPA, data=dat) 
summary(m.c)
vif(m.c) #no issues of collinearity here, remove confounding variable
plot(m.c)

#testing for outliers in DASScore
Das1<-lm(CognitionZscore~ DASScore, data=dat) 
Das1<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
write.csv(Das1,row.names = FALSE,"DASScore.outliers.csv")

install.packages("arm")
library(arm)
display(m.c)

m.d<-lm(CognitionZscore~ DepressionScore+AnxietyScore+StressScore, data=dat)
vif(m.d)
summary(m.d)
plot(m.d)
#Test goodness of fit for LOGISTIC using: significance of coefficients, -2 log lieklihood/ deviance, classification table
#test for linear: R sq adj R sq, F stat, adj SE, coeff fit, residual plot
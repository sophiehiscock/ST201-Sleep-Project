#dat<-read.csv("sleepedit.csv",header=T) #pre-edited data, without aliased coefficients
dat<-read.csv("sleep2.csv", header=T) #missing first 10 rows so we can test with this later
head(dat)


#Testing variables from each group 

dat$LarkOwl<-relevel(dat$LarkOwl,ref="Neither")
m5<-lm(CognitionZscore~Gender + LarkOwl + NumEarlyClass + ClassesMissed + PoorSleepQuality + PoorSleepQuality*ClassesMissed + GPA + DASScore + Drinks + WeekdayRise , data=dat)

summary(m5)


fullm5<-lm(CognitionZscore~ Gender + LarkOwl + NumEarlyClass + ClassesMissed + PoorSleepQuality + PoorSleepQuality*ClassesMissed + GPA + Happiness + Drinks + WeekdayRise , data=dat)

summary(fullm5)


m6<-lm(CognitionZscore~ Gender + NumEarlyClass + ClassesMissed + PoorSleepQuality + PoorSleepQuality*ClassesMissed + GPA + DASScore + Drinks + WeekdayRise + NumEarlyClass*GPA, data=dat)
summary(m6)

anova(m6)

m7<-lm(CognitionZscore~ Gender + NumEarlyClass + ClassesMissed + PoorSleepQuality + PoorSleepQuality*ClassesMissed + GPA + StressScore + Drinks + WeekdayRise + NumEarlyClass*GPA, data=dat)
summary(m7)
anova(m7)

m8<-lm(CognitionZscore~ Gender + NumEarlyClass + ClassesMissed + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + StressScore + Drinks + WeekdayRise + NumEarlyClass*GPA, data=dat)
summary(m8)

m9<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + ClassesMissed + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality+ StressScore + Drinks + WeekdayRise + NumEarlyClass*GPA, data=dat)
summary(m9)

new.dat<-read.csv("ExtraData.csv")

predict(m9, newdata=new.dat, interval='confidence')

pred.int <- predict(m9, interval = "prediction")
mydata <- cbind(dat, pred.int)
# 2. Regression line + confidence intervals
library("ggplot2")
p <- ggplot(mydata, aes(x=all.vars(), CognitionZscore)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")



#Building from m9
m10<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + ClassesMissed + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality+ StressScore + Drinks + WeekdayRise + WeekdayBed + NumEarlyClass*GPA, data=dat)
summary(m10)

predict(m10, newdata=new.dat, interval='confidence')

#Building from m10 - exchanging stress for anxiety 
m11<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + ClassesMissed + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality+ AnxietyScore + Drinks + WeekdayRise + WeekdayBed + NumEarlyClass*GPA, data=dat)
summary(m11)

#Building from m11 
m12<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality+ StressScore + Drinks + WeekdayRise + WeekdayBed + NumEarlyClass*GPA, data=dat)
summary(m12)

predict(m12, newdata=new.dat, interval='confidence')

#Building from m12
#Best Model So Far
m13<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality+ StressScore + Drinks + WeekdayBed + WeekdayRise + WeekdayRise*GPA+ NumEarlyClass*GPA, data=dat)
summary(m13)

predict(m13, newdata=new.dat, interval='confidence')

#_____________________________#
#Analysing modle m13 

#LINEARITY?
m13.stdres<- rstandard(m13)
with(dat, plot(GPA, m13.stdres))
with(dat, plot(StressScore, m13.stdres))
with(dat, plot(ClassesMissed, m13.stdres))
with(dat, plot(EarlyClass*ClassesMissed, m13.stdres))
with(dat, plot(WeekdayBed, m13.stdres))

#NORMALITY?
#histogram
hist(m13.stdres)
plot(density(m13.stdres))
#Q-Q plot 
plot(m4, which=c(2))
#P-P plot
probDist <- pnorm(m13.stdres)
plot(ppoints(length(m13.stdres)), sort(probDist), main = 'PP Plot', xlab = 'Observed Probability', ylab = 'Expected Probability')

#CONSTANT VARIANCE?
plot(m13, which=c(1))

#INDEPENDENCE?
plot(m13.stdres)

#Other areas to test

#COLLINEARITY?
#correlation matrix, >|0.8|?
dtf <- subset(dat, select = c(StressScore,ClassesMissed,GPA,EarlyClass,NumEarlyClass,Gender,WeekdayRise,WeekdayBed))
cor(dtf[sapply(dtf, is.numeric)])
cor(dat$NumEarlyClass,dat$EarlyClass) #0.8089492
#scatterplot matrix
pairs(dtf[sapply(dtf, is.numeric)])
#VIF, >10? serious problem
install.packages('car')
library(car)
vif(m4) #NumEarlyClass, EarlyClass and their product~3


#Testing Data 
install.packages("sigr")

rand <- sample(1:254, 20, replace = FALSE)
test <- dat[rand, ]
a <- predict(m4,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)

#d<-data.frame(prediction=test$CognitionZscore,actual=dat$CognitionZscore)
#cor.test(d$prediction,d$actual)

rand <- sample(1:254, 20, replace = FALSE)
test <- dat[rand, ]
a <- predict(m4,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)


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




#building from m3
m3<-lm(CognitionZscore~ WeekdayRise+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+NumEarlyClass+Gender, data=dat)
summary(m3)

m4<-lm(CognitionZscore~ NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
summary(m4)














#Partial F tests__________________________________________________________

#StressScore*Gender
full1<-lm(CognitionZscore~ StressScore*Gender+NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
anova(full1,m4)

#compare with m1
summary(m1)
anova(m1,m4) #SIGNIFICANT YASSSSS

#AlcoholUse
full2<-lm(CognitionZscore~factor(AlcoholUse)+ClassYear+NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
anova(full2,m4)
summary(full2)

#Drinks
full3<-lm(CognitionZscore~Drinks+ClassYear+NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
anova(full3,m4)

#AverageSleep
full4<-lm(CognitionZscore~AverageSleep+ClassYear+NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
anova(full4,m4)

#testing assumptions on m3______________________________________________________

#LINEARITY?
m4.stdres<- rstandard(m4)
with(dat, plot(GPA, m4.stdres))
with(dat, plot(StressScore, m4.stdres))
with(dat, plot(ClassesMissed, m4.stdres))
with(dat, plot(EarlyClass*ClassesMissed, m3.stdres))

#NORMALITY?
#histogram
hist(m4.stdres)
plot(density(m4.stdres))
#Q-Q plot 
plot(m4, which=c(2))
#P-P plot
probDist <- pnorm(m4.stdres)
plot(ppoints(length(m4.stdres)), sort(probDist), main = 'PP Plot', xlab = 'Observed Probability', ylab = 'Expected Probability')

#CONSTANT VARIANCE?
plot(m4, which=c(1))

#INDEPENDENCE?
plot(m4.stdres)

#Other areas to test

#COLLINEARITY?
#correlation matrix, >|0.8|?
dtf <- subset(dat, select = c(StressScore,ClassesMissed,GPA,EarlyClass,NumEarlyClass,Gender))
cor(dtf[sapply(dtf, is.numeric)])
cor(dat$NumEarlyClass,dat$EarlyClass) #0.8089492
#scatterplot matrix
pairs(dtf[sapply(dtf, is.numeric)])
#VIF, >10? serious problem
install.packages('car')
library(car)
vif(m4) #NumEarlyClass, EarlyClass and their product~3


#Testing heteroskedasticity 
dat$resi<-m4$residuals
library(ggplot2)
ggplot(data=dat, aes_(y = CognitionZScore, x = all.vars()) + geom_point(col = 'blue') + geom_abline(slope = 0))
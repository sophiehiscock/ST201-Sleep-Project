#dat<-read.csv("sleepedit.csv",header=T) #pre-edited data, without aliased coefficients
dat<-read.csv("sleepcut.csv", header=T) #missing first 10 rows so we can test with this later
head(dat)

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

#PoorSleepQuality*ClassesMissed not significant
full5<-lm(CognitionZscore~PoorSleepQuality*ClassesMissed+AverageSleep+ClassYear+NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
anova(full5,m4)

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
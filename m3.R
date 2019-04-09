#dat<-read.csv("sleepedit.csv",header=T) #pre-edited data, without aliased coefficients
dat<-read.csv("sleepcut.csv", header=T) #missing first 10 rows so we can test with this later
head(dat)

#building from m2
m2<-lm(CognitionZscore~ WeekdayRise+DepressionStatus+StressScore+ClassesMissed+GPA+EarlyClass+NumEarlyClass+Gender, data=dat)
summary(m2)

m3<-lm(CognitionZscore~ WeekdayRise+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+NumEarlyClass+Gender, data=dat)
summary(m3)

#Partial F tests__________________________________________________________

#removing WeekdayRise
full1<-lm(CognitionZscore~ NumEarlyClass+StressScore+ClassesMissed+EarlyClass*ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
anova(m3,full1)
summary(full1)

#testing assumptions on m3______________________________________________________

#LINEARITY?
m3.stdres<- rstandard(m3)
with(dat, plot(GPA, m3.stdres))
with(dat, plot(StressScore, m3.stdres))
with(dat, plot(WeekdayRise, m3.stdres))
with(dat, plot(ClassesMissed, m3.stdres))
with(dat, plot(EarlyClass*ClassesMissed, m3.stdres))

#NORMALITY?
#histogram
hist(m3.stdres)
plot(density(m3.stdres))
#Q-Q plot 
plot(m3, which=c(2))
#P-P plot
probDist <- pnorm(m3.stdres)
plot(ppoints(length(m3.stdres)), sort(probDist), main = 'PP Plot', xlab = 'Observed Probability', ylab = 'Expected Probability')

#CONSTANT VARIANCE?
plot(m3, which=c(1))

#INDEPENDENCE?
plot(m3.stdres)

#Other areas to test

#COLLINEARITY?
#correlation matrix, >|0.8|?
dtf <- subset(dat, select = c(WeekdayRise,StressScore,GPA,EarlyClass,NumEarlyClass,Gender))
cor(dtf[sapply(dtf, is.numeric)])
cor(dat$EarlyClass*dat$ClassesMissed,dtf) #no problems
cor(dat$NumEarlyClass,dat$EarlyClass) #0.8089492

#scatterplot matrix
pairs(dtf[sapply(dtf, is.numeric)])
#VIF, >10? serious problem
install.packages('car')
library(car)
vif(m3)
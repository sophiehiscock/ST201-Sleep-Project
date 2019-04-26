#dat<-read.csv("sleepedit.csv",header=T) #pre-edited data, without aliased coefficients
dat<-read.csv("sleep.csv", header=T) #missing first 10 rows so we can test with this later
head(dat)

#starting point of subsets
m2<-lm(CognitionZscore~ WeekdayRise+DepressionStatus+StressScore+ClassesMissed+GPA+EarlyClass+NumEarlyClass+Gender, data=dat)
summary(m2)

#Partial F tests__________________________________________________________

#removing NumEarlyClass
full1<-lm(CognitionZscore~ WeekdayRise+DepressionStatus+StressScore+ClassesMissed+GPA+EarlyClass+Gender,  data=dat)
anova(m2,full1)

#removing DepressionStatus
full2<-lm(CognitionZscore~ WeekdayRise+StressScore+ClassesMissed+GPA+EarlyClass+NumEarlyClass+Gender,  data=dat)
anova(full2,m2)

#Drinks
full3<-lm(CognitionZscore~ Drinks+WeekdayRise+DepressionStatus+StressScore+ClassesMissed+GPA+EarlyClass+NumEarlyClass+Gender,  data=dat)
anova(m2,full3)

#removing ClassesMissed
full4<-lm(CognitionZscore~ WeekdayRise+DepressionStatus+StressScore+GPA+EarlyClass+NumEarlyClass+Gender,  data=dat)
anova(full4,m2)

#removing EarlyClass
full5<-lm(CognitionZscore~ WeekdayRise+DepressionStatus+StressScore+ClassesMissed+GPA+NumEarlyClass+Gender,  data=dat)
anova(full5,m2)

#NumEarlyClass*ClassesMissed
full6<-lm(CognitionZscore~ EarlyClass*ClassesMissed+WeekdayRise+DepressionStatus+StressScore+ClassesMissed+GPA+EarlyClass+NumEarlyClass+Gender,  data=dat)
anova(full6,m2)

#testing assumptions on m2______________________________________________________

#LINEARITY?
m2.stdres<- rstandard(m2)
with(dat, plot(GPA, m2.stdres))
with(dat, plot(StressScore, m2.stdres))
with(dat, plot(WeekdayRise, m2.stdres))
with(dat, plot(ClassesMissed, m2.stdres))

#NORMALITY?
#histogram
hist(m2.stdres)
plot(density(m2.stdres))
#Q-Q plot 
plot(m2, which=c(2))
#P-P plot
probDist <- pnorm(m2.stdres)
plot(ppoints(length(m2.stdres)), sort(probDist), main = 'PP Plot', xlab = 'Observed Probability', ylab = 'Expected Probability')

#CONSTANT VARIANCE?
plot(m2, which=c(1))

#INDEPENDENCE?
plot(m2.stdres)

#Other areas to test

#COLLINEARITY?
#correlation matrix, >|0.8|?
dtf <- subset(dat, select = c(WeekdayRise,StressScore,ClassesMissed,GPA,EarlyClass,NumEarlyClass,Gender))
cor(dtf[sapply(dtf, is.numeric)])
cor(dat$NumEarlyClass,dat$EarlyClass) #NumEarlyClass and EarlyClass: 0.8089492
#scatterplot matrix
pairs(dtf[sapply(dtf, is.numeric)])
#VIF, >10? serious problem
install.packages('car')
library(car)
vif(m2)


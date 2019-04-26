#dat<-read.csv("sleepedit.csv",header=T) #pre-edited data, without aliased coefficients
dat<-read.csv("sleep.csv", header=T) 
head(dat)

#starting point of backwards stepwise
m1<-lm(CognitionZscore ~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore, data = dat)
summary(m1) #this model looks really good!!! all variables significant :)

#Partial F tests__________________________________________________________

#DepressionStatus
full1<-lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore+ DepressionStatus,  data=dat)
anova(m1,full1)

#WeekdayRise
full2<-lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore+ WeekdayRise,  data=dat)
anova(m1, full2)

#CLassesMissed
full3<-lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore+ ClassesMissed,  data=dat)
anova(m1, full3)

#AnxietyStatus
full4<-lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore+ AnxietyStatus,  data=dat)
anova(m1,full4)

#ClassYear
full5<-lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore+ ClassYear,  data=dat)
anova(m1,full5)

#GPA*Gender
full6<-lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore+ GPA*Gender,  data=dat)
anova(m1,full6)

#AverageSleep
full7<-lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore+ AverageSleep,  data=dat)
anova(m1,full7)

#testing assumptions on m1______________________________________________________

#LINEARITY?
m1.stdres<- rstandard(m1)
with(dat, plot(GPA, m1.stdres))
with(dat, plot(StressScore, m1.stdres))

#NORMALITY?
  #histogram
hist(m1.stdres)
plot(density(m1.stdres))
  #Q-Q plot 
plot(m1, which=c(2))
  #P-P plot
probDist <- pnorm(m1.stdres)
plot(ppoints(length(m1.stdres)), sort(probDist), main = 'PP Plot', xlab = 'Observed Probability', ylab = 'Expected Probability')

#CONSTANT VARIANCE?
plot(m1, which=c(1))

#INDEPENDENCE?
plot(m1.stdres)

#Other areas to test

#COLLINEARITY?
  #correlation matrix, >|0.8|?
cor(dat$StressScore,dat$GPA)
  #scatterplot matrix
pairs(~GPA + StressScore, data=dat)
  #VIF, >10? serious problem
install.packages('car')
library(car)
vif(m1)


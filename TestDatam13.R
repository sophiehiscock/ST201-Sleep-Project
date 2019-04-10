
dat<-read.csv("sleep3.csv", header=T) #missing randomly selected 30 pieces of data
head(dat)


m13<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality + StressScore + Drinks + WeekdayBed + WeekdayRise + WeekdayRise*GPA + NumEarlyClass*GPA, data=dat)
summary(m13)
#Important note: the p-value of the F statistic increases (not as small as we would like it to be) once observations are removed. Could be important to highlight!

#Other potential models 
m14<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality + DASScore + Drinks + WeekdayBed + WeekdayRise + WeekdayRise*GPA + NumEarlyClass*GPA, data=dat)
summary(m14)

m15<- lm(CognitionZscore~ Gender + NumEarlyClass + EarlyClass + PoorSleepQuality + GPA+ PoorSleepQuality*GPA + NumEarlyClass*PoorSleepQuality + StressScore + Drinks + WeekdayBed + WeekdayRise + WeekdayRise*GPA + NumEarlyClass*GPA, data=dat)
summary(m13)
#__________________________________#


#Analysing the model

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
#d<-data.frame(prediction=test$CognitionZscore,actual=dat$CognitionZscore)
#cor.test(d$prediction,d$actual)

new.dat<-read.csv("TestData.csv")

predict(m13, newdata=new.dat, interval='confidence')


rand <- sample(1:30, 10, replace = FALSE)
test <- new.dat[rand, ]
a <- predict(m13,newdata=test,interval='confidence')
cor(test$CognitionZscore, a)



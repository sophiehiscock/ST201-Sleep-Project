#dat<-read.csv("sleepedit.csv",header=T) #pre-edited data, without aliased coefficients
dat<-read.csv("sleepcut.csv", header=T) #missing first 10 rows so we can test with this later
head(dat)
dat$DepressionStatus<-relevel(dat$DepressionStatus, ref="moderate")

#stepwise methods for initial exploration

#forwards
null=lm(CognitionZscore~1,data=dat)
full=lm(CognitionZscore~.,data=dat)
step(null, scope=list(lower=null, upper=full),direction="forward")
#lm(formula = CognitionZscore ~ GPA + Gender + StressScore + ClassesMissed + AnxietyStatus, data = dat)
mforwards<-lm(CognitionZscore ~ GPA + Gender + StressScore + ClassesMissed + AnxietyStatus, data = dat)
summary(mforwards)

#backwards
step(full,direction="backward")
#lm(formula = CognitionZscore ~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore, data = dat)
mbackward<-lm(CognitionZscore ~ Gender + NumEarlyClass + EarlyClass + GPA + StressScore, data = dat)
summary(mbackward) #this model looks really good!!! all variables significant :)

#subset
install.packages('leaps')
library(leaps)
leaps<-regsubsets(CognitionZscore ~.-DASScore,data = dat)
plot(leaps,scale="adjr2")
#suggests WeekdayRise DepressionStatusSevere StressScore ClassesMissed GPA EarlyClass NumEarlyClass Gender
msubsets<-lm(CognitionZscore~ WeekdayRise +factor(DepressionStatus)+StressScore+ClassesMissed+GPA+EarlyClass+NumEarlyClass+Gender, data=dat)
summary(msubsets) #looks good based on R squared value

#looking for colinearity
install.packages('car')
library(car)
vif(msubsets)
vif(mforwards)
vif(mbackward)

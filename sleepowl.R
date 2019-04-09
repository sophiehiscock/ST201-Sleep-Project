dat<-read.csv("owlsleep.csv", header=T) #missing first 10 rows so we can test with this later

head(dat)

#testing for effectiveness of later classes__________________________________________
owl<-lm(CognitionZscore~EarlyClass+NumEarlyClass, data=dat)
summary(owl)
#library(arm)
#outliers

x<-1:20
y<-jitter(x+2,20)
plot(x,y)
lm1<-lm(y~x)

#change y[12] to an outlier
y[12]<-30
plot(x,y)
points(x[12],y[12],col="red")
lm2<-lm(y~x)

abline(lm1)
abline(lm2, col="red")

display(lm1)
display(lm2)


#add y[21]
y[12]<-x[12]+2
x<-c(1:20,35)
y[21]<-x[21]+2
plot(x,y)
points(x[21],y[21],col="red")

lm3<-lm(y~x)
abline(lm1)
abline(lm3,col="red")
display(lm3)

#change y[21]
y[21]<-10
plot(x,y)
points(x[21],y[21],col="red")

lm4<-lm(y~x)
abline(lm1)
abline(lm4,col="red")


lm2.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm2),2),Lev=round(hatvalues(lm2),2),Cooks=round(cooks.distance(lm2),2)))
lm3.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm3),2),Lev=round(hatvalues(lm3),2),Cooks=round(cooks.distance(lm3),2)))
lm4.outliers<-data.frame(Stand.Res=cbind(round(rstandard(lm4),2),Lev=round(hatvalues(lm4),2),Cooks=round(cooks.distance(lm4),2)))

write.csv(lm2.outliers,row.names = FALSE,"lm2.csv")
write.csv(lm3.outliers,row.names = FALSE,"lm3.csv")
write.csv(lm4.outliers,row.names = FALSE,"lm4.csv")

plot(lm4,which=c(4))

test.dat<-data.frame(x=x,y=y)

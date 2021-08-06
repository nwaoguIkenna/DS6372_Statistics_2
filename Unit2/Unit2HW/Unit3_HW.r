par(mfrow=c(1,2))
plot(Auto$horsepower,log(Auto$mpg), xlab="horsepower",ylab="logmpg",col=cylinders)
new<-data.frame(horsepower=seq(30,300,.1))
horse.model2<-lm(log(Auto$mpg)~Auto$horsepower)
lines(seq(30,300,.1),predict(horse.model2,newdata=new),col="red",lwd=4)
plot(horse.model2$fitted.values,horse.model2$residuals,xlab="Fitted Values",ylab="Residuals")
plot(horsepower,horse.model2$residuals,xlab="Horsepower",ylab="Residuals")

par(mfrow=c(1,2))
plot(horse.model2)

summary(horse.model2)


library(caret)
library(dplyr) 
Auto <- Auto%>% filter(cylinders!='3' | cylinders!='5')
Auto <- Auto%>% select(mpg,displacement,horsepower,weight,acceleration,year,origin)
set.seed(1234)
splitPerc = .5 #Training / Test split Percentage
trainIndices = sample(1:dim(Auto)[1],round(splitPerc * dim(Auto)[1]))
train = Auto[trainIndices,]
test = Auto[-trainIndices,]

library(leaps)
reg.fwd=regsubsets(log(mpg)~.,data=train,method="forward",nvmax=7)
summary(reg.fwd)
coef(reg.fwd,5)
summary(reg.fwd)$adjr2
summary(reg.fwd)$rss
summary(reg.fwd)$bic

par(mfrow=c(1,3))
bics<-summary(reg.fwd)$bic
plot(1:7,bics,type="l",ylab="BIC",xlab="# of predictors")
index<-which(bics==min(bics))
points(index,bics[index],col="red",pch=10)

adjr2<-summary(reg.fwd)$adjr2
plot(1:7,adjr2,type="l",ylab="Adjusted R-squared",xlab="# of predictors")
index<-which(adjr2==max(adjr2))
points(index,adjr2[index],col="red",pch=10)

rss<-summary(reg.fwd)$rss
plot(1:7,rss,type="l",ylab="train RSS",xlab="# of predictors")
index<-which(rss==min(rss))
points(index,rss[index],col="red",pch=10)

predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

testASE<-c()
#note my index is to 20 since that what I set it in regsubsets
for (i in 1:7){
  predictions<-predict.regsubsets(object=reg.fwd,newdata=test,id=i) 
  testASE[i]<-mean((log(test$mpg)-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:7,testASE,type="l",xlab="# of predictors",ylab="test ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red")
rss<-summary(reg.fwd)$rss
lines(1:7,rss/100,lty=3,col="blue")  #Dividing by 100 since ASE=RSS/sample size


final.model<-lm(log(mpg)~horsepower+weight+year,data=Auto)
summary(final.model)

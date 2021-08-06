reg.fwd=regsubsets(all ~ aroma + appearance + palate + abv + taste  + abv*taste + abv*palate + palate* taste,data=regData, nvmax = 8,
                              method = "seqrep")
summary(reg.fwd)
coef(reg.fwd,8)
summary(reg.fwd)$adjr2
summary(reg.fwd)$rss
summary(reg.fwd)$bic

par(mfrow=c(1,3))
bics<-summary(reg.fwd)$bic
plot(1:8,bics,type="l",ylab="BIC",xlab="# of predictors")
index<-which(bics==min(bics))
points(index,bics[index],col="red",pch=10)

adjr2<-summary(reg.fwd)$adjr2
plot(1:8,adjr2,type="l",ylab="Adjusted R-squared",xlab="# of predictors")
index<-which(adjr2==max(adjr2))
points(index,adjr2[index],col="red",pch=10)

rss<-summary(reg.fwd)$rss
plot(1:8,rss,type="l",ylab="train RSS",xlab="# of predictors")
index<-which(rss==min(rss))
points(index,rss[index],col="red",pch=10)

  predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}



predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

testASE<-c()
#note my index is to 20 since that what I set it in regsubsets
for (i in 1:8){
  predictions<-predict.regsubsets(object=reg.fwd,newdata=test,id=i) 
  testASE[i]<-mean((test$all-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:8,testASE,type="l",xlab="# of predictors",ylab="test ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red")
  rss<-summary(reg.fwd)$rss
lines(1:8,rss/100,lty=3,col="blue")  #Dividing by 100 since ASE=RSS/sample size

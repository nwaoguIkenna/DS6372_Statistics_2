library(dplyr) 
library(plotly) 
library(car)  
setwd("~/datascience/DS6372/ProjectDetails")
reviews<-read.csv("beer_reviews.csv")
reviews
names(reviews)

#regData <- reviews %>%select("review_overall","review_time","review_aroma","review_appearance","review_palate","review_taste","beer_name",beer_style,beer_abv,brewery_name)
regData <- reviews %>% filter(!is.na(beer_abv)) %>% group_by(brewery_name,beer_name) %>% summarise(all = mean(review_overall),aroma = mean(review_aroma), appearance = mean(review_appearance), palate = mean(review_palate),abv = mean(beer_abv), taste = mean(review_taste),count = n())

regData %>% arrange(-count)

regData <- regData %>% select(beer_name,all, aroma, appearance, palate, abv, taste, count)
reg <- head(regData,5000)

regData %>% ggplot(aes(y = all, x = abv)) + geom_point()
pairs(regData[2:8])

# Cross Validation 75 train and 25 test..
splitPerc = 0.75
set.seed(124)
trainIndices = sample(1:dim(regData)[1],round(splitPerc * dim(regData)[1]))
train = regData[trainIndices,]
test = regData[-trainIndices,]

model1 = lm(all ~ aroma + appearance + palate + abv + taste + abv*palate + abv*taste + abv*appearance, data = train)
model2 = lm(all ~ aroma + appearance + palate + log(abv) + taste + log(abv)*taste, data = train)
model3 = lm(all ~ aroma + appearance + palate + abv + taste + abv*taste, data = train)
model4 = lm(all ~ aroma + appearance + palate + abv + taste + abv*palate, data = train)
model5 = lm(all ~ aroma + appearance + palate + abv + taste + abv*appearance, data = train)
model6 = lm(all ~ aroma + appearance + palate + abv + taste + abv*aroma, data = train)
model7 = lm(all ~ aroma + appearance + palate + abv + taste, data = train)
model8 = lm(all ~ taste + abv, data = train)
model9 = lm(all ~ log(abv), data = train)
model10 = lm(all ~ taste + aroma, data = train)

predict = predict(model7, test)
RMSE = sqrt(mean((test$all - predict)^2))
RMSE






anova(model1, model2, model3, model4, model5,model6, model7, model8, model9, model10)
#######################################################################

model7 %>% ggplot(aes(model7$fitted.values,model7$residuals)) + geom_point()
summary(model7)
qqnorm(model7$residuals)
qqline(model7$residuals)


plot(test$all,predict)

par(mfrow=c(1,2))
plot(model3)

confint(model3, level =0.95)

#####################################################################

#write.csv(reg, file = '~/datascience/DS6372/ProjectDetails/proj1.csv')

#####################################################################
library(leaps)
reg.fwd=regsubsets(all ~ aroma + appearance + palate + abv + taste,data=train,method="forward",nvmax=7)
summary(reg.fwd)
coef(reg.fwd,5)
summary(reg.fwd)$adjr2
summary(reg.fwd)$rss
summary(reg.fwd)$bic

par(mfrow=c(1,3))
bics<-summary(reg.fwd)$bic
plot(1:5,bics,type="l",ylab="BIC",xlab="# of predictors")
index<-which(bics==min(bics))
points(index,bics[index],col="red",pch=10)

adjr2<-summary(reg.fwd)$adjr2
plot(1:5,adjr2,type="l",ylab="Adjusted R-squared",xlab="# of predictors")
index<-which(adjr2==max(adjr2))
points(index,adjr2[index],col="red",pch=10)

rss<-summary(reg.fwd)$rss
plot(1:5,rss,type="l",ylab="train RSS",xlab="# of predictors")
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
for (i in 1:5){
  predictions<-predict.regsubsets(object=reg.fwd,newdata=test,id=i) 
  testASE[i]<-mean((test$all-predictions)^2)
}
par(mfrow=c(1,1))
plot(1:5,testASE,type="l",xlab="# of predictors",ylab="test ASE")
index<-which(testASE==min(testASE))
points(index,testASE[index],col="red")
rss<-summary(reg.fwd)$rss
lines(1:5,rss/100,lty=3,col="blue")  #Dividing by 100 since ASE=RSS/sample size

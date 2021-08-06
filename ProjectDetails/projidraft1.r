library(dplyr) 
library(plotly) 
setwd("~/datascience/DS6372/ProjectDetails")
reviews<-read.csv("beer_reviews.csv")
reviews
names(reviews)

regData <- reviews %>%select("review_overall",beer_style,beer_abv) %>% filter(beer_style == 'American IPA' | beer_style == 'American Double / Imperial IPA' | beer_style == 'American Pale Ale (APA)') %>% filter(!is.na(beer_abv))
regData1 <- reviews %>%select("review_overall",beer_style,beer_abv,brewery_name) %>% filter(beer_style == 'American IPA' | beer_style == 'American Double / Imperial IPA' | beer_style == 'American Pale Ale (APA)') %>% filter(!is.na(beer_abv))
regData2 <- reviews %>%select("review_overall", beer_abv) %>% filter(!is.na(beer_abv))

regdata3 <- reviews %>%select("review_overall","review_time","review_aroma","review_appearance","review_palate","review_taste","beer_name",beer_style,beer_abv,brewery_name)
regdata3 <- regdata3  %>% filter(!is.na(beer_abv)) %>% group_by(brewery_name,beer_name) %>% summarise(all = mean(review_overall),aroma = mean(review_aroma), appearance = mean(review_appearance), palate = mean(review_palate),abv = mean(beer_abv), taste = mean(review_taste),count = n())

#regdata3 %>% filter(fct_explicit_na (brewery_name, na_level = 'missing'))
regdata3 %>% arrange(-count)

regData3 <- regdata3 %>% select(all, aroma, appearance, palate, abv, taste, count)
reg <- head(regdata3,5000)


count(regData)
summary(regData)

regData %>% ggplot(aes(x = beer_style, fill = beer_style)) + geom_bar()
regData %>% ggplot(aes(x = beer_style, y = beer_abv)) + geom_point()
regData %>% ggplot(aes(x = beer_style, y = review_overall)) + geom_boxplot()
regData %>% ggplot(aes(y = review_overall, x = beer_abv)) + geom_point()
#regData %>% ggplot(aes(x = brewery_name, y = review_overall)) + geom_boxplot()

summary(regData)
  
regData1 %>% group_by(brewery_name) %>% summarise(count = n()) %>% arrange(-count)

regData1 <- regData1 %>% filter(brewery_name == 'Sierra Nevada Brewing Co.' | brewery_name == 'Dogfish Head Brewery'| brewery_name == 'Stone Brewing Co.')


library(car)  

#########################################################################


##############################################################
trainIndices = sample(1:dim(regData)[1],round(splitPerc * dim(regData)[1]))
train = regData[trainIndices,]
test = regData[-trainIndices,]

model = lm(review_overall ~ ., data = train)
predict = predict(model, test)
RMSE = sqrt(mean((test$review_overall - predict)^2))
RMSE
#######################################################################

#new<-data.frame(beer_style=seq(30,300,.1),beer_abv=seq(30,300,.1))
#lines(seq(30,300,.1),predict(model,newdata=new),col="red",lwd=4)
model %>% ggplot(aes(model$fitted.values,model$residuals)) + geom_point()
summary(model)
qqnorm(model$residuals)



##############################################################
trainIndices = sample(1:dim(regData1)[1],round(splitPerc * dim(regData1)[1]))
train = regData1[trainIndices,]
test = regData1[-trainIndices,]

model1 = lm(review_overall ~ ., data = train)
predict = predict(model, test)
RMSE = sqrt(mean((test$review_overall - predict)^2))
RMSE
#######################################################################

model1 %>% ggplot(aes(model1$fitted.values,model1$residuals)) + geom_point()
summary(model1)
qqnorm(model1$residuals)



##############################################################

trainIndices = sample(1:dim(regData2)[1],round(splitPerc * dim(regData2)[1]))
train = regData2[trainIndices,]
test = regData2[-trainIndices,]

model2 = lm(review_overall ~ ., data = train)
predict = predict(model, test)
RMSE = sqrt(mean((test$review_overall - predict)^2))
RMSE
#######################################################################

model2 %>% ggplot(aes(model2$fitted.values,model2$residuals)) + geom_point()
summary(model2)
qqnorm(model2$residuals)


##########################################################################





trainIndices = sample(1:dim(regData3)[1],round(splitPerc * dim(regData3)[1]))
train = regData3[trainIndices,]
test = regData3[-trainIndices,]

model3 = lm(all ~ aroma + appearance + palate + abv + taste + abv*palate + abv*taste + abv*appearance + abv*aroma, data = train)
model3 = lm(all ~ aroma + appearance + palate + log(abv) + taste + log(abv)*taste, data = train)
model3 = lm(all ~ aroma + appearance + palate + abv + taste + abv*palate, data = train)
model3 = lm(all ~ aroma + appearance + palate + abv + taste + abv*appearance, data = train)
model3 = lm(all ~ aroma + appearance + palate + abv + taste + abv*aroma, data = train)
model3 = lm(all ~ taste + abv*taste, data = train)
model3 = lm(all ~ log(abv), data = train)
predict = predict(model3, test)
RMSE = sqrt(mean((test$all - predict)^2))
RMSE
#######################################################################

model3 %>% ggplot(aes(model3$fitted.values,model3$residuals)) + geom_point()
summary(model3)
qqnorm(model3$residuals)
qqline(model3$residuals)


plot(test$all,predict)

#####################################################################
pairs(regData3[,c(2,8)])

write.csv(reg, file = '~/datascience/DS6372/ProjectDetails/proj1.csv')

#####################################################################
library(leaps)
reg.fwd=regsubsets(log(mpg)~displacement+horsepower+weight+acceleration+year+origin,data=train,method="forward",nvmax=7)



pairs(regData3[2:8])
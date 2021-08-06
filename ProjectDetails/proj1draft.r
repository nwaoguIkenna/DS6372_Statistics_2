library(dplyr) 
library(plotly) 
setwd("~/datascience/DS6372/ProjectDetails")
reviews<-read.csv("beer_reviews.csv")
reviews
names(reviews)

reviews %>% select("brewery_name","review_overall","review_aroma","review_appearance","review_profilename","review_palate","review_taste") %>% 
  filter(brewery_name == 'Vecchio Birraio')

#reviews %>% select("brewery_name","review_overall","review_time","beer_style", beer_name) %>% filter(brewery_name == 'Caldera Brewing Company')


#reviews %>% filter(brewery_name == 'Caldera Brewing Company') %>% group_by(beer_name) %>% summarise(overall = mean(review_overall), aroma = mean(review_aroma))

#reviews  %>% group_by(brewery_name, beer_name) %>% summarise(overall = mean(review_overall), aroma = mean(review_aroma))

e <- reviews %>%select("review_overall","review_aroma","review_appearance","review_palate","review_taste", beer_abv) %>% filter(!is.na(beer_abv))

cor(e)

reviews[grep('Bud L',reviews$beer_name),]

e <- reviews  %>% group_by(beer_style) %>% summarise(count = n()) %>% arrange(-count)
##e <- count(reviews, beer_style)%>% arrange(n)    #This works too.


regData <- reviews %>%select("review_overall",beer_style,beer_abv) %>% filter(beer_style == 'American IPA' | beer_style == 'American Double / Imperial IPA' | beer_style == 'American Pale Ale (APA)') %>% filter(!is.na(beer_abv))

regData %>% filter(beer_style == 'American IPA' & beer_abv != 6.1)

count(regData)
summary(regData)

par(mfrow=c(1,3))
plot(regData$beer_style,regData$beer_abv, xlab="Beer Style",ylab="ABV")

regData %>% ggplot(aes(x = beer_style, fill = beer_style)) + geom_bar()
regData %>% ggplot(aes(x = beer_style, y = beer_abv)) + geom_point()
regData %>% ggplot(aes(x = beer_style, y = review_overall)) + geom_boxplot()
regData %>% ggplot(aes(y = review_overall, x = beer_abv)) + geom_point()

regData %>% filter(beer_style != 'American IPA' & beer_style != 'American Double / Imperial IPA')# | beer_style != 'American Pale Ale (APA)')

regData$beer_style <-  as.character(regData$beer_style) 

regData1 <- regData %>% mutate( beer_style = replace(beer_style, beer_style == 'American IPA', '1'))
regData1 <- regData1 %>% mutate( beer_style = replace(beer_style, beer_style == 'American Double / Imperial IPA', '2'))
regData1 <- regData1 %>% mutate( beer_style = replace(beer_style, beer_style == 'American Pale Ale (APA)', '3'))

regData$beer_style <-  as.factor(regData$beer_style) 
regData1$beer_style <-  as.factor(regData1$beer_style) 

regData1$beer_style <-  as.numeric(regData1$beer_style)
regData1$review_overall <-  as.numeric(regData1$review_overall)

#cor(regData1)

regData1$beer_style <-  as.factor(regData1$beer_style)
regData1$review_overall <-  as.factor(regData1$review_overall)


#library(car)  
model <- lm(review_overall~.,data=regData1)  # . means all variable not mpg

#regData$review_overall <-  as.factor(regData$review_overall)
#model1 <- lm(review_overall~.,data=regData)
#vif(model)[,3]^2

#regData2 <- reviews %>%select("review_overall",beer_style,beer_abv) %>% filter(!is.na(beer_abv))
#model2 <- lm(review_overall~.,data=regData2)


#########################################################################

stat <- data.frame()

splitPerc = 0.75
for (i in 1:40){
  set.seed(i)
  #count = count + 1
  trainIndices = sample(1:dim(regData)[1],round(splitPerc * dim(regData)[1]))
  train = regData[trainIndices,]
  test = regData[-trainIndices,]
  
  model = lm(review_overall ~ ., data = train)
  predict = predict(model, test)
  RMSE = sqrt(mean((test$review_overall - predict)^2))
  ID = i
  d = data.frame(ID,RMSE)
  stat <- bind_rows(stat,d)
}

stat %>% filter(RMSE==max(RMSE))
stat %>% filter(RMSE==min(RMSE))

mean(stat$RMSE)

##############################################################
trainIndices = sample(1:dim(regData)[1],round(splitPerc * dim(regData)[1]))
train = regData[trainIndices,]
test = regData[-trainIndices,]

model = lm(review_overall ~ ., data = train)
predict = predict(model, test)
RMSE = sqrt(mean((test$review_overall - predict)^2))
RMSE
#######################################################################

###Time Series
#time <- reviews %>% filter(beer_name == 'Bud Light') %>% select(beer_name,review_overall,review_time,trans = as.POSIXct.numeric(reviews$review_time,origin='1970-01-01 00:00:00',tz='EST'))
time <- reviews %>% filter(beer_name == 'Bud Light') %>% select(beer_name,review_overall,review_time)
time <- time%>% mutate(beer_name, time = as.POSIXct.numeric(review_time,origin='1970-01-01 00:00:00',tz='EST'))
time <- time %>% arrange(review_time)
time

###fct_rev() forcats package
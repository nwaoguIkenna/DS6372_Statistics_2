library(dplyr) 
library(plotly) 
library(leaps)
library(MASS)
library(caret)
library(olsrr)
library(car)

setwd("~/datascience/DS6372/ProjectDetails")
reviews<-read.csv("beer_reviews.csv")
names(reviews)

regData <- reviews %>% filter(!is.na(beer_abv)) %>% group_by(brewery_name,beer_name) %>% summarise(all = mean(review_overall),aroma = mean(review_aroma), appearance = mean(review_appearance), palate = mean(review_palate),abv = mean(beer_abv), taste = mean(review_taste),count = n())
names(regData)

regData %>% arrange(-count)

#regData <- regData %>% dplyr::select("beer_name",all, aroma, appearance, palate, abv, taste, count)

reg <- head(regData,5000)  # This was 5000 data used for student version of SAS because it is only the limit.

pairs(regData[3:9]) # this is to compare all the points including count(no of reviews per beer)

hist(regData$count)

# we can eliminate count from the pairs and also from SAS output

# Cross Validation 75 train and 25 test..
#splitPerc = 0.75

# Set seed for reproducibility
set.seed(124)   

#trainIndices = sample(1:dim(regData)[1],round(splitPerc * dim(regData)[1]))
#train = regData[trainIndices,]
#test = regData[-trainIndices,]
model7 = lm(all ~ aroma + appearance + palate + abv + taste, data = regData)

summary(model7)

k <- ols_step_both_aic(model7)
plot(k)

full.model <- lm(all ~ aroma + appearance + palate + abv + taste  + abv*taste + abv*palate + palate* taste,data=regData)


## All possible regression model and the values they provide.
all <- ols_step_all_possible(full.model)
View(all)
plot(all) #This displays all the attributes of the all data into a plot showing which ones are the better values.

# Best suset of regression model
subset <- ols_step_best_subset(full.model)
View(subset) # This shows all the best model based on the values of a given parameter
plot(subset)  # This displays all the parameters on a plot

# AIC forward
forw <- ols_step_forward_aic(full.model)
plot(forw)
#AIC backward
bac <- ols_step_backward_aic(full.model)
plot(bac)
#AIC both
k <- ols_step_both_aic(full.model)
plot(k)


# Stepwise regression model for AIC
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)    # The stars in the parenthesis means that it is involved in the model.

# forward regression model for AIC
step.model <- stepAIC(full.model, direction = "forward", 
                      trace = FALSE)
summary(step.model)

# Backward regression model For AIC
step.model <- stepAIC(full.model, direction = "backward", 
                      trace = FALSE)
summary(step.model)

model1 = lm(all ~ aroma + appearance + palate + abv + taste + abv*palate + abv*taste + taste*palate, data = regData)
confint(model1)

# Model selection using regsusets. We can use below to support the subset of the earlier model
# selection using stepwise
models <- regsubsets(all ~ aroma + appearance + palate + abv + taste  + abv*taste + abv*palate + palate* taste,data=regData, nvmax = 8,
                     method = "seqrep")
summary(models)

# Model Selection using forward
models <- regsubsets(all ~ aroma + appearance + palate + abv + taste  + abv*taste + abv*palate + palate* taste,data=regData, nvmax = 8,
                     method = "forward")
summary(models)

# Model Selection using Backward
models <- regsubsets(all ~ aroma + appearance + palate + abv + taste  + abv*taste + abv*palate + palate* taste,data=regData, nvmax = 8,
                     method = "backward")
summary(models)

# Model selection using K-fold cross validation and train() of the caret package
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model using backward
step.model <- train(all ~ aroma + appearance + palate + abv + taste  + abv*taste + abv*palate + palate* taste,data=regData,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:8),
                    trControl = train.control
)
step.model$results

step.model$bestTune

summary(step.model$finalModel)

# Train the model using forward
step.model <- train(all ~ aroma + appearance + palate + abv + taste  + abv*taste + abv*palate + palate* taste,data=regData,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:8),
                    trControl = train.control
)
step.model$results

step.model$bestTune

summary(step.model$finalModel)

# Train the model using stepwise
step.model <- train(all ~ aroma + appearance + palate + abv + taste  + abv*taste + abv*palate + palate* taste,data=regData,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:8),
                    trControl = train.control
)
step.model$results

step.model$bestTune

summary(step.model$finalModel)

# As you can see each and every model selection gave us different models look at those and let me know what you think. I think either one of 5,7,8 is good.
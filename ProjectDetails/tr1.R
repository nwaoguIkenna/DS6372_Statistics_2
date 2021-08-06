library(MASS)
library(caret)
# Fit the full model 
splitPerc = 0.75
set.seed(124)
trainIndices = sample(1:dim(regData)[1],round(splitPerc * dim(regData)[1]))
train = regData[trainIndices,]
test = regData[-trainIndices,]
full.model <- lm(all ~ aroma + appearance + palate + abv + taste  + abv*taste,data=train)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)




train.control <- trainControl(method = "cv", number = 10)
step.model <- train(all ~ aroma + appearance + palate + abv + taste + abv*taste,data=train,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)

step.model$results

step.model$bestTune

summary(step.model$finalModel)

#######################################################################
library(olsrr)
data <- ols_step_all_possible(full.model)

View(data)
options(max.print=60)
print.data.frame(data) 

ols_step_best_subset(full.model)
#########################################################################

train.control <- trainControl(method = "cv", number = 10)
step.model <- train(all ~ aroma + appearance + palate + abv + taste,data=train,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:6),
                    trControl = train.control
)

step.model$results

step.model$bestTune

summary(step.model$finalModel)






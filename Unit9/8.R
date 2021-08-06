
library(MASS)
library(mvtnorm)
set.seed(1234)
dataYes<-mvrnorm(30,c(10,10),matrix(c(1,.8,.8,1),2,2,byrow=T))
dataNo<- mvrnorm(30,c(10,7),matrix(c(1,.8,.8,1),2,2,byrow=T))
full<-rbind(dataYes,dataNo)
full<-data.frame(full)
full$Response<-rep(c("Yes","No"),each=30)
full$Response<-factor(full$Response)
names(full)[1:2]<-c("X1","X2")
plot(full[, 1:2], col = full$Response, main="Shift in X2")

# construct the LDA model
mylda <- lda(Response ~ X1*X2 + X2*X1, data = full)

# draw discrimination line
np <- 300
 nd.x <- seq(from = min(full$X1), to = max(full$X1), length.out = np)
nd.y <- seq(from = min(full$X2), to = max(full$X2), length.out = np)
nd <- expand.grid(X1 = nd.x, X2 = nd.y)

prd <- as.numeric(predict(mylda, newdata = nd)$class)

plot(full[, 1:2], col = full$Response, main="Shift in X2")
points(mylda$means, pch = "+", cex = 2, col = c("black", "red"))
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE)

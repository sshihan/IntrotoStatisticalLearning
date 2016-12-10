# Assignment 10
# Shaheed Shihan
# Modern Applied Statistics

#6
par(xpd = NA)
plot(NA, NA, type = "n", xlim = c(-2, 2), ylim = c(-3, 3), xlab = "X1", ylab = "X2")
# X2 < 1
lines(x = c(-2, 2), y = c(1, 1))
# X1 < 1 with X2 < 1
lines(x = c(1, 1), y = c(-3, 1))
text(x = (-2 + 1)/2, y = -1, labels = c(-1.8))
text(x = 1.5, y = -1, labels = c(0.63))
# X2 < 2 with X2 >= 1
lines(x = c(-2, 2), y = c(2, 2))
text(x = 0, y = 2.5, labels = c(2.49))
# X1 < 0 with X2<2 and X2>=1
lines(x = c(0, 0), y = c(1, 2))
text(x = -1, y = 1.5, labels = c(-1.06))
text(x = 1, y = 1.5, labels = c(0.21))


#8
# In the lab, a classification tree was applied to the Carseats data set after
# converting Sales into a qualitative response variable. Now we will
# seek to predict Sales using regression trees and related approaches,
# treating the response as a quantitative variable.
# (a) Split the data set into a training set and a test set.

library(ISLR)
library(tree)
attach(Carseats)
head(Carseats)

set.seed(222)
train <- sample(nrow(Carseats),nrow(Carseats)-nrow(Carseats)/3)
d.train <- Carseats[train,]
d.test <- Carseats[-train,]
head(d.train)

dim(d.train)

# (b) Fit a regression tree to the training set. Plot the tree, and interpret
# the results. What test MSE do you obtain?

tree.carseats <- tree(Sales~., data = d.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
yhat <- predict(tree.carseats, newdata = d.test)
mean((yhat - d.test$Sales)^2)

# (c) Use cross-validation in order to determine the optimal level of
# tree complexity. Does pruning the tree improve the test MSE?
set.seed(123)
cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
min.tree <- which.min(cv.carseats$dev)
points(min.tree, cv.carseats$dev[min.tree], col = "red", cex = 2, pch =20)
##minimum occurs at 8


prune.carseats <- prune.tree(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

yhat2 <- predict(prune.carseats, newdata = d.test)
mean((yhat2 - d.test$Sales)^2)

# (d) Use the bagging approach in order to analyze this data. What
# test MSE do you obtain? Use the importance() function to determine
# which variables are most important.

library(randomForest)
set.seed(1234)
bag.cs <- randomForest(Sales~., data = d.train, mtry = 10, ntree = 1000,
                       importance = TRUE)
yhat3 <- predict(bag.cs, newdata = d.test)
mean((yhat3 - d.test$Sales)^2)
importance(bag.cs)
varImpPlot(bag.cs)

# (e) Use random forests to analyze this data. What test MSE do you
# obtain? Use the importance() function to determine which variables
# aremost important. Describe the effect of m, the number of
# variables considered at each split, on the error rate
# obtained.

##Creating a loop to go through all m's from 2-6.
m <- 2:6
##Creating a table with the results
tab <- matrix(NA, nrow = length(m), ncol = 1)
rownames(tab) <- m

##Loop
for (i in 1:length(m)){
  rf.cs <- randomForest(Sales~. , data = d.train, mtry = m[i],
                        importance = TRUE)
  yhat4 <- predict(rf.cs, newdata = d.test)
  tab[i,1] <- mean((yhat4 - d.test$Sales)^2)
  }

tab
importance(rf.cs)
varImpPlot(rf.cs)

detach(Carseats)

# 9. This problem involves the OJ data set which is part of the ISLR
# package.
# (a) Create a training set containing a random sample of 800 observations,
# and a test set containing the remaining observations.

attach(OJ)
head(OJ)
dim(OJ)
train <- sample(1:nrow(OJ), 800) 
train.oj <- OJ[train,]
test.oj <- OJ[-train,]

dim(train.oj)
# (b) Fit a tree to the training data, with Purchase as the response
# and the other variables as predictors. Use the summary() function
# to produce summary statistics about the tree, and describe the
# results obtained. What is the training error rate? How many
# terminal nodes does the tree have?

tree.oj <- tree(Purchase~., data = train.oj)
summary(tree.oj)


# (c) Type in the name of the tree object in order to get a detailed
# text output. Pick one of the terminal nodes, and interpret the
# information displayed.

tree.oj

# (d) Create a plot of the tree, and interpret the results.
plot(tree.oj)
text(tree.oj, pretty = 0)

# (e) Predict the response on the test data, and produce a confusion
# matrix comparing the test labels to the predicted test labels.
# What is the test error rate?

pred <- predict(tree.oj, newdata = test.oj, type = "class")
table(pred, test.oj$Purchase)
1-sum(diag(table(pred,test.oj$Purchase)))/nrow(test.oj)

# (f) Apply the cv.tree() function to the training set in order to
# determine the optimal tree size.

cv.oj <- cv.tree(tree.oj, FUN = prune.misclass)
cv.oj


# (g) Produce a plot with tree size on the x-axis and cross-validated
# classification error rate on the y-axis.
plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", 
     ylab = "Deviance", main = "Tree size against Deviance")

# (h) Which tree size corresponds to the lowest cross-validated classification
# error rate?
cv.oj$dev

# (i) Produce a pruned tree corresponding to the optimal tree size
# obtained using cross-validation. If cross-validation does not lead
# to selection of a pruned tree, then create a pruned tree with five
# terminal nodes.

oj.prune <- prune.misclass(tree.oj, best = 4)
plot(oj.prune)
text(oj.prune, pretty = 0)

# (j) Compare the training error rates between the pruned and unpruned
# trees. Which is higher?
summary(oj.prune)
summary(tree.oj)

# (k) Compare the test error rates between the pruned and unpruned
# trees. Which is higher?

pred1 <- predict(oj.prune, newdata = test.oj, type = "class")
table(pred1, test.oj$Purchase)
1-sum(diag(table(pred1,test.oj$Purchase)))/nrow(test.oj)

detach(OJ)
# 10. We now use boosting to predict Salary in the Hitters data set.
# (a) Remove the observations for whom the salary information is
# unknown, and then log-transform the salaries.
attach(Hitters)
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)

# (b) Create a training set consisting of the first 200 observations, and
# a test set consisting of the remaining observations.

train <- 1:200
H.train <- Hitters[train,]
H.test <- Hitters[-train,]

head(H.train)
# (c) Perform boosting on the training set with 1,000 trees for a range
# of values of the shrinkage parameter λ. Produce a plot with
# different shrinkage values on the x-axis and the corresponding
# training set MSE on the y-axis.
library(gbm)
set.seed(4)
lambda <- c(0.00001,0.0001,0.001,0.05,0.01,0.1,
            0.2,0.3,0.4,0.5,0.6,1)
train.err <- rep(NA, length(lambda))

for (i in 1:length(lambda)){
  l = lambda[i]
  hitters.boost <- gbm(Salary~. , data = H.train, distribution = "gaussian",
                       n.trees = 1000,
                       shrinkage = l,
                       interaction.depth = 1,
                       n.cores = 10)
  yhatboost <- predict(hitters.boost, newdata = H.test, n.trees = 1000)
  train.err[i] <- mean((yhatboost-H.test$Salary)^2)
  
}
plot(lambda, train.err, type = "b")

# (d) Produce a plot with different shrinkage values on the x-axis and
# the corresponding test set MSE on the y-axis.
test.err <- rep(NA, length(lambda))

for (i in 1:length(lambda)){
  l = lambda[i]
  hitters.boost <- gbm(Salary~. , data = H.train, distribution = "gaussian",
                       n.trees = 1000,
                       shrinkage = l,
                       interaction.depth = 1,
                       n.cores = 10)
  yhatboost <- predict(hitters.boost, newdata = H.test, n.trees = 1000)
  test.err[i] <- mean((yhatboost-H.test$Salary)^2)
  
}
plot(lambda, test.err, type = "b")
min(test.err)
lambda[which.min(test.err)]

# (e) Compare the test MSE of boosting to the test MSE that results
# from applying two of the regression approaches seen in
# Chapters 3 and 6.

glm.fit <- glm(Salary~. , data = H.train)
pred2 <- predict(glm.fit, H.test)
mean((pred2-H.test$Salary)^2)

library(pls)
pcr.fit <- pcr(Salary~. , data = H.train)
pred3 <- predict(pcr.fit, H.test)
mean((pred3-H.test$Salary)^2)

# (f) Which variables appear to be the most important predictors in
# the boosted model?
hitters.boost <- gbm(Salary~. , data = H.train, distribution = "gaussian",
                     n.trees = 1000,
                     shrinkage = lambda[which.min(test.err)])
summary(hitters.boost)


# (g) Now apply bagging to the training set. What is the test set MSE
# for this approach?

set.seed(56)
h.bag <- randomForest(Salary~., data = H.train, mtry =19,ntree =500)
yhatbag <- predict(h.bag,newdata = H.test)
mean((yhatbag-H.test$Salary)^2)



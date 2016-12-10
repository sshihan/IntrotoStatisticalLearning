# # Assignment 12
# # Shaheed Shihan
# # Modern Applied Statistics
# 
# 2. We have seen that in p = 2 dimensions, a linear decision boundary
# takes the form β0+β1X1+β2X2 = 0. We now investigate a non-linear
# decision boundary.
# (a) Sketch the curve
# (1 + X1)2 + (2 − X2)2 = 4.

plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)


# (b) On your sketch, indicate the set of points for which
# (1 + X1)2 + (2 − X2)2 > 4,
# as well as the set of points for which 
# (1 + X1)2 + (2 − X2)2 ≤ 4.

plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")


# (c) Suppose that a classifier assigns an observation to the blue class
# if
# (1 + X1)2 + (2 − X2)2 > 4,
# and to the red class otherwise. To what class is the observation
# (0, 0) classified? (−1, 1)? (2, 2)? (3, 8)?

plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)


# (d) Argue that while the decision boundary in (c) is not linear in
# terms of X1 and X2, it is linear in terms of X1, X2
# 1 , X2, and
# X2


# 7. In this problem, you will use support vector approaches in order to
# predict whether a given car gets high or low gas mileage based on the
# Auto data set.
# (a) Create a binary variable that takes on a 1 for cars with gas
# mileage above the median, and a 0 for cars with gas mileage
# below the median.
library(ISLR)
library(e1071)

data("Auto")
#head(Auto)
attach(Auto)
mileage <- as.factor(ifelse(mpg > median(mpg), 1,0))
Auto <- cbind(Auto, mileage)
#head(Auto) 
#str(Auto) 

# (b) Fit a support vector classifier to the data with various values
# of cost, in order to predict whether a car gets high or low gas
# mileage. Report the cross-validation errors associated with different
# values of this parameter. Comment on your results.

set.seed(1) 
tunelinear <- tune(svm,mileage~.,data=Auto,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100))) 
summary(tunelinear) 
bestmod1 <- tunelinear$best.model 
summary(bestmod1) 
# 
# (c) Now repeat (b), this time using SVMs with radial and polynomial
# basis kernels, with different values of gamma and degree and
# cost. Comment on your results.

set.seed(1)
tunerad <-tune(svm, mileage~., data=Auto, kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))

summary(tunerad)
bestmod2 <- tunerad$best.model
summary(bestmod2)

set.seed(1)
tunepol <- tune(svm, mileage ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4)))
summary(tunepol)
bestmod3 <- tunepol$best.model
summary(bestmod3)

# (d) Make some plots to back up your assertions in (b) and (c).
# Hint: In the lab, we used the plot() function for svm objects
# only in cases with p = 2. When p > 2, you can use the plot()
# function to create plots displaying pairs of variables at a time.
# Essentially, instead of typing
# > plot(svmfit , dat)
# where svmfit contains your fitted model and dat is a data frame
# containing your data, you can type
# > plot(svmfit , dat , x1∼x4)
# in order to plot just the first and fourth variables. However, you
# must replace x1 and x4 with the correct variable names. To find
# out more, type ?plot.svm.

fit <- svm(mileage~., data = Auto, kernel = "linear", cost = 1)
fit2 <- svm(mileage~., data = Auto, kernel = "polynomial",cost = 100, degree = 2)
fit3 <- svm(mileage~. , data = Auto, kernel = "radial", cost = 10, gamma = 0.5)

plotp <-  function(f){
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mileage","name"))]) {
    plot(f, Auto, as.formula(paste("mpg~", name, sep="")))
  }
}
plotp(fit)
plotp(fit2)
plotp(fit3)

detach(Auto)

# 8. This problem involves the OJ data set which is part of the ISLR
# package.
# (a) Create a training set containing a random sample of 800
# observations, and a test set containing the remaining
# observations.
attach(OJ)
#head(OJ)
#dim(OJ)
train <- sample(nrow(OJ), 800)
trainOJ <- OJ[train, ]
testOJ <- OJ[-train, ]


# (b) Fit a support vector classifier to the training data using
# cost=0.01, with Purchase as the response and the other variables
# as predictors. Use the summary() function to produce summary
# statistics, and describe the results obtained.
set.seed(4)
linearsvm <- svm(Purchase~. , data = trainOJ, kernel = "linear", cost = 0.01)
summary(linearsvm)

# At a cost of 0.01, 448 points out of 800 were used 
# for creating the support vector. 224 of these belong to MM level and the other 224 to the CH level. 

# (c) What are the training and test error rates?
predtrain <- predict(linearsvm, trainOJ)
t <- table(predtrain, trainOJ$Purchase)
1-sum(diag(t))/800


##test set
predtest <- predict(linearsvm, testOJ)
t1 <- table(predtest,testOJ$Purchase)
1-sum(diag(t1))/270


# (d) Use the tune() function to select an optimal cost. Consider values
# in the range 0.01 to 10.
set.seed(3)
tunelinear1 <- tune(svm,Purchase~. , data=trainOJ, kernel="linear",ranges=list(cost=c(0.01, 0.05, 0.1, 0.25, 0.5,0.75,
                                                                             1,5,10))) 
summary(tunelinear1)

# (e) Compute the training and test error rates using this new value
# for cost.

tunedsvm <- svm(Purchase~. , data = trainOJ, kernel = "linear", cost = 10)
pred1 <- predict(tunedsvm, trainOJ)
t <- table(pred1, trainOJ$Purchase)
1-sum(diag(t))/800

tunedsvm <- svm(Purchase~. , data = testOJ, kernel = "linear", cost = 10)
pred1 <- predict(tunedsvm, testOJ)
t <- table(pred1, testOJ$Purchase)
1-sum(diag(t))/270



# (f) Repeat parts (b) through (e) using a support vector machine
# with a radial kernel. Use the default value for gamma.


set.seed(4)
radialsvm <- svm(Purchase~. , data = trainOJ, kernel = "radial", cost = 0.01)
summary(radialsvm)

# At a cost of 0.01, 653 points out of 800 were used 
# for creating the support vector. 328 of these belong to MM level and the other 325 to the CH level. 

#training and test error rates?
pred2 <- predict(radialsvm, trainOJ)
t <- table(pred2, trainOJ$Purchase)
1-sum(diag(t))/800


##test set
predtest <- predict(radialsvm, testOJ)
t1 <- table(predtest,testOJ$Purchase)
1-sum(diag(t1))/270


set.seed(3)
tuneradial1 <- tune(svm,Purchase~. , data=trainOJ, kernel="radial",ranges=list(cost=c(0.01, 0.05, 0.1, 0.25, 0.5,0.75,
                                                                                      1,5,10))) 
summary(tuneradial1)

tunedsvm <- svm(Purchase~. , data = trainOJ, kernel = "radial", cost = 1)
pred1 <- predict(tunedsvm, trainOJ)
t <- table(pred1, trainOJ$Purchase)
1-sum(diag(t))/800

pred1 <- predict(tunedsvm, testOJ)
t <- table(pred1, testOJ$Purchase)
1-sum(diag(t))/270


# (g) Repeat parts (b) through (e) using a support vector machine
# with a polynomial kernel. Set degree=2.

set.seed(5)
polysvm <- svm(Purchase~. , data = trainOJ, kernel = "polynomial", degree = 2)
summary(polysvm)

# At a cost of 1 and degree of 2, 463 points out of 800 were used 
# for creating the support vector. 234 of these belong to MM level and the other 229 to the CH level. 

#training and test error rates?
pred4 <- predict(polysvm, trainOJ)
t <- table(pred4, trainOJ$Purchase)
1-sum(diag(t))/800


##test set
predtest <- predict(polysvm, testOJ)
t1 <- table(predtest,testOJ$Purchase)
1-sum(diag(t1))/270


set.seed(3)
tunepoly1 <- tune(svm,Purchase~. , data=trainOJ, kernel="polynomial",degree = 2,ranges=list(cost=c(0.01, 0.05, 0.1, 0.25, 0.5,0.75,
                                                                                      1,5,10))) 
summary(tunepoly1)

tunedsvm <- svm(Purchase~. , data = trainOJ, kernel = "polynomial", cost = 10, degree = 2)
pred1 <- predict(tunedsvm, trainOJ)
t <- table(pred1, trainOJ$Purchase)
1-sum(diag(t))/800

pred1 <- predict(tunedsvm, testOJ)
t <- table(pred1, testOJ$Purchase)
1-sum(diag(t))/270

# (h) Overall, which approach seems to give the best results on this data?
# My lowest test error comes from the linear kernel but overall they all seem to perform quite well 
# with error rates around 16%. 
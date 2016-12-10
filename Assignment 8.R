##Assignment 9

library(ISLR)
library(glmnet)
library(pls)
data(College)
head(College)
attach(College)
# (a) Split the data set into a training set and a test set.

set.seed(1111)
train <- sample(nrow(College),nrow(College)-nrow(College)/3)
test <- (!train)
C.train <- College[train, ]
C.test <- College[-train, ]

# (b) Fit a linear model using least squares on the training set, and
# report the test error obtained.

lm.C <- lm(Apps~., data = C.train)
pred.lm <- predict(lm.C, C.test)
mean((pred.lm-C.test$Apps)^2)

##Calculating R-squared value
test.mean <- mean(C.test$Apps)
r.lm <- 1 - mean((pred.lm-C.test$Apps)^2)/mean((test.mean - C.test$Apps)^2)

# (c) Fit a ridge regression model on the training set, with λ chosen
# by cross-validation. Report the test error obtained.
mat.train <- model.matrix(Apps~., data = C.train)
mat.test <- model.matrix(Apps~., data = C.test)
g <- 10^seq(10,-2,length = 100)
ridge.C <- glmnet(mat.train, C.train$Apps, alpha = 0, lambda = g, thresh = 1e-12)
ridge.cv <- cv.glmnet(mat.train, C.train$Apps, alpha = 0, lambda = g, thresh = 1e-12)
b.ridge <- ridge.cv$lambda.min
b.ridge
#best lambda by cv = 18.73817
#Predicting on test values
ridge.pred <- predict(ridge.C, s = b.ridge, newx = mat.test)
mean((ridge.pred - C.test$Apps)^2)

r.ridge <- 1 - mean((ridge.pred - C.test$Apps)^2)/mean((test.mean - C.test$Apps)^2)


# (d) Fit a lasso model on the training set, with λ chosen by crossvalidation.
# Report the test error obtained, along with the number of non-zero coefficient estimates.

lasso.C <- glmnet(mat.train, C.train$Apps, alpha = 1, lambda = g, thresh = 1e-12)
lasso.cv <- cv.glmnet(mat.train, C.train$Apps, alpha = 1, lambda = g, thresh = 1e-12)
b.lasso <- lasso.cv$lambda.min
b.lasso
lasso.pred <- predict(lasso.C, s = b.lasso, newx = mat.test)
mean((lasso.pred - C.test$Apps)^2)

##Non zero coefficients
predict(lasso.C, s = b.lasso, type = "coefficients")

r.lasso <- 1 - mean((lasso.pred - C.test$Apps)^2)/mean((test.mean - C.test$Apps)^2)


# (e) Fit a PCR model on the training set, with M chosen by crossvalidation.
# Report the test error obtained, along with the value of M selected by cross-validation.
pcr.C <- pcr(Apps~., data = C.train, scale = T, validation = "CV")
validationplot(pcr.C, val.type = "MSEP")

##Predicting
pcr.pred <- predict(pcr.C, C.test, ncomp = 10)
mean((pcr.pred - C.test$Apps)^2)

r.pcr <- 1 - mean((pcr.pred - C.test$Apps)^2)/mean((test.mean - C.test$Apps)^2)

# (f) Fit a PLS model on the training set, with M chosen by crossvalidation. 
#Report the test error obtained, along with the value of M selected by cross-validation.
pls.C <- plsr(Apps~. , data = C.train, scale = T, validation = "CV")
validationplot(pls.C, val.type = "MSEP")

##Predicting
pls.pred <- predict(pls.C, C.test, ncomp = 10)
mean((pls.pred - C.test$Apps)^2)

r.pls <- 1 - mean((pls.pred - C.test$Apps)^2)/mean((test.mean - C.test$Apps)^2)


# (g) Comment on the results obtained. How accurately can we predict
# the number of college applications received? Is there much
# difference among the test errors resulting from these five approaches?

data.frame(r.lm,r.ridge, r.lasso,r.pcr,r.pls)

detach(College)


###11
library(MASS)
data("Boston")
set.seed(122)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

k = 10
folds <- sample(1:k, nrow(Boston), replace = TRUE)
cv.errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))
for (j in 1:k) {
  fit1 <- regsubsets(crim ~ ., data = Boston[folds != j, ], nvmax = 13)
  for (i in 1:13) {
    pred <- predict(fit1, Boston[folds == j, ], id = i)
    cv.errors[j, i] <- mean((Boston$crim[folds == j] - pred)^2)
  }
}
mean.cverrors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors, type = "b", xlab = "Number of variables", ylab = "CV error")


##lasso
a <- model.matrix(crim ~ ., Boston)[, -1]
b <- Boston$crim
cv.lasso <- cv.glmnet(a,b, alpha = 1, type.measure = "mse")
plot(cv.lasso)

##ridge
cv.ridge <- cv.glmnet(a,b, alpha = 0, type.measure = "mse")
plot(cv.ridge)

##pcr
fitpcr <- pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
summary(fitpcr)
validationplot(fitpcr, val.type = "MSEP")

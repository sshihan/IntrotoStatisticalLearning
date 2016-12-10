# Assignment 9
# Shaheed Shihan
# Modern Applied Statistics
# 
# 6. In this exercise, you will further analyze the Wage data set considered 
# throughout this chapter.
# (a) Perform polynomial regression to predict wage using age. Use
# cross-validation to select the optimal degree d for the polynomial.
# What degree was chosen, and how does this compare to 
# the results of hypothesis testing using ANOVA? Make a plot of
# the resulting polynomial fit to the data.

library(ISLR)
library(boot)
library(MASS)
attach(Wage)
set.seed(123)
deltas <- rep(NA,10)
for (i in 1:10){
  fit <- glm(wage ~ poly(age,i), data = Wage)
  deltas[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
  }
plot(1:10, deltas, xlab = "Degree", ylab = "Test Mean Square Error", type = "l", main = "MSE for degree of Polynomial")
grid(col = "purple")
mindegree <- which.min(deltas)
points(mindegree,deltas[mindegree], col = "blue", cex = 3, pch =20)

##Running ANOVA
fit1 <- lm(wage ~ age, data = Wage)
fit2 <- lm(wage ~ poly(age, 2), data = Wage)
fit3 <- lm(wage ~ poly(age, 3), data = Wage)
fit4 <- lm(wage ~ poly(age, 4), data = Wage)
fit5 <- lm(wage ~ poly(age, 5), data = Wage)
fit6 <- lm(wage ~ poly(age, 6), data = Wage)
fit7 <- lm(wage ~ poly(age, 7), data = Wage)
fit8 <- lm(wage ~ poly(age, 8), data = Wage)
fit9 <- lm(wage ~ poly(age, 9), data = Wage)
fit10 <- lm(wage ~ poly(age, 10), data = Wage)
anova(fit1, fit2, fit3, fit4, fit5,fit6,fit7,fit8,fit9,fit10)

### P- value explanation needed. Book's explanation differs from my understanding. 

##Plotting the data
plot(wage ~ age, data = Wage, col = "darkgrey", main = "Polynomial fit of Age against Wage")
grid(col = "purple")
limitage <- range(Wage$age)
age.grid <- seq(from = limitage[1], to = limitage[2])

fit <- lm(wage ~ poly(age, 2), data = Wage)
preds <- predict(fit, newdata = list(age = age.grid))
lines(age.grid, preds, col = "yellow", lwd = 2)

fit_1 <- lm(wage ~ poly(age, 3), data = Wage)
preds <- predict(fit_1, newdata = list(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 2)

fit_2 <- lm(wage ~ poly(age, 4), data = Wage)
preds <- predict(fit_2, newdata = list(age = age.grid))
lines(age.grid, preds, col = "blue", lwd = 2)

fit_3 <- lm(wage ~ poly(age, 9), data = Wage)
preds <- predict(fit_3, newdata = list(age = age.grid))
lines(age.grid, preds, col = "green", lwd = 2)


# (b) Fit a step function to predict wage using age, and perform crossvalidation
# to choose the optimal number of cuts. Make a plot of
# the fit obtained.

deltas2 <- rep(NA, 10)
for (i in 2:10) {
  Wage$age.cut <- cut(Wage$age, i) ##converting to factor by cutting into i # of breaks
  fit <- glm(wage ~ age.cut, data = Wage) #fitting a glm model 
  deltas2[i] <- cv.glm(Wage, fit, K = 10)$delta[1] #Running 10 fold cv on the glm model
}
plot(2:10, deltas2[-1], xlab = "Cuts", ylab = "Test Mean Square Error", type = "l", main = "Optimal number of cuts")
grid(col = "purple")
mindegree2 <- which.min(deltas2)
points(mindegree2, deltas2[mindegree2], col = "blue", cex = 3, pch = 20)


##Plotting the cut
plot(wage ~ age, data = Wage, col = "darkgrey", main = "Wage against Age with 8 cuts")
grid(col = "purple")
fit <- glm(wage ~ cut(age, 8), data = Wage)
preds <- predict(fit, data.frame(age = age.grid))
lines(age.grid, preds, col = "blue", lwd = 2)

detach(Wage)

# 9. This question uses the variables dis (the weighted mean of distances
# to five Boston employment centers) and nox (nitrogen oxides concentration                                                                                     in parts per 10 million) from the Boston data. We will treat
# dis as the predictor and nox as the response.
# (a) Use the poly() function to fit a cubic polynomial regression to
# predict nox using dis. Report the regression output, and plot
# the resulting data and polynomial fits.

attach(Boston)
head(Boston)
fit <- lm(nox ~ poly(dis, 3), data = Boston)
summary(fit)

##Plotting
xlimit <- range(Boston$dis) ##setting limits
dis.grid <- seq(from = xlimit[1], to = xlimit[2], by = 0.1)
preds <- predict(fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey", main = "Cubic Polynomial plot of nox vs dis")
grid(col = "purple")
lines(dis.grid, preds, col = "blue", lwd = 2)

# (b) Plot the polynomial fits for a range of different polynomial
# degrees (say, from 1 to 10), and report the associated residual
# sum of squares.

resids <- rep(NA, 15)
for (i in 1:15) {
  fit <- lm(nox ~ poly(dis, i), data = Boston)
  resids[i] <- sum(fit$residuals^2)
}
plot(1:15, resids, xlab = "Degree", 
     ylab = "Residual Sum of Squares", 
     type = "l", 
     main = "Polynomial fit for a range of Polynomial degrees")
grid(col = "purple")
mindegree <- which.min(resids)
points(mindegree,resids[mindegree], col = "blue", cex = 3, pch =20)



# (c) Perform cross-validation or another approach to select the optimal
# degree for the polynomial, and explain your results.
set.seed(124)
deltas3 <- rep(NA, 10)
for (i in 1:10) {
  bfit <- glm(nox ~ poly(dis, i), data = Boston)
  deltas3[i] <- cv.glm(Boston, bfit, K = 10)$delta[1]
}
plot(1:10, deltas3, xlab = "Degree", ylab = "Test MSE", type = "l",main = "Mean square error for various polynomial fits")
grid(col = "purple")

# (d) Use the bs() function to fit a regression spline to predict nox
# using dis. Report the output for the fit using four degrees of
# freedom. How did you choose the knots? Plot the resulting fit.
set.seed(4)
fit_bs <- lm(nox ~ bs(dis, df = 4), data = Boston)
summary(fit_bs)

sp.pred <-  predict(fit_bs, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey", main = "Regression Spline to predict nox using dis")
grid(col = "purple")
lines(dis.grid, sp.pred, col = "blue", lwd = 2)
# (e) Now fit a regression spline for a range of degrees of freedom, and
# plot the resulting fits and report the resulting RSS. Describe the
# results obtained.

resids2 <- rep(NA, 16)
for (i in 3:16) {
  fit <- lm(nox ~ bs(dis, df = i), data = Boston)
  resids2[i] <- sum(fit$residuals^2)
}
plot(3:16, resids2[-c(1, 2)], xlab = "Degrees of freedom", 
     ylab = "RSS", type = "l", 
     main = "Regression spline with varying degrees of freedom")
grid(col = "purple")


# (f) Perform cross-validation or another approach in order to select
# the best degrees of freedom for a regression spline on this data.
# Describe your results.
set.seed(3)
cv <- rep(NA, 16)
for (i in 3:16) {
  fit <- glm(nox ~ bs(dis, df = i), data = Boston)
  cv[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(3:16, cv[-c(1, 2)], xlab = "Degrees of freedom", ylab = "Test MSE", type = "l", 
     main = "Mean Square Error for various degrees of freedom using CV")
grid(col = "purple")


# 10. This question relates to the College data set.
# (a) Split the data into a training set and a test set. Using out-of-state
# tuition as the response and the other variables as the predictors,
# perform forward stepwise selection on the training set in order
# to identify a satisfactory model that uses just a subset of the
# predictors.

library(leaps)
set.seed(1)
attach(College)
train <- sample(length(Outstate), length(Outstate) * 0.75)
test <- -train
College.train <- College[train, ]
College.test <- College[test, ]
fit <- regsubsets(Outstate ~ ., data = College.train, nvmax = 17, method = "forward")
fit.summary <- summary(fit)

par(mfrow = c(1, 3))
##Plotting  Mallows' CP
plot(fit.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l", main = "Mallow's Cp")
grid(col = "purple")
min.cp <- min(fit.summary$cp)
std.cp <- sd(fit.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "blue", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "blue", lty = 2)

##Plotting Schwartz's Information Criterion 
plot(fit.summary$bic, xlab = "Number of variables", ylab = "BIC", type='l', 
     main = "Schwartz's Information Criterion")
grid(col = "purple")
min.bic <- min(fit.summary$bic)
std.bic <- sd(fit.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "green", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "green", lty = 2)

##Plotting Adjusted R-squared value
plot(fit.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R2", type = "l", ylim = c(0.4, 0.84),
     main = "Adjusted R-squared value")
grid(col = "purple")
max.adjr2 <- max(fit.summary$adjr2)
std.adjr2 <- sd(fit.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)

##finding the coefficients
fit <- regsubsets(Outstate ~ ., data = College, method = "forward")
coeffs <- coef(fit, id = 6)
names(coeffs)

##Why are we choosing 6 and not 11 or 12 where BIC and Cp are minimized? Overfitting maybe?
##number of variables minimizing cp and bic
which.min (fit.summary$cp)
which.min (fit.summary$bic)

# (b) Fit a GAM on the training data, using out-of-state tuition as
# the response and the features selected in the previous step as
# the predictors. Plot the results, and explain your findings.
library(gam)
gam.fit <-  gam(Outstate ~ Private + s(Room.Board, df=2) + 
                  s(PhD, df=2) + s(perc.alumni, df=2) + 
                  s(Expend, df=5) + s(Grad.Rate, df=2),
                data=College.train)
par(mfrow=c(2, 3))
plot(gam.fit, se=TRUE, col="blue")
grid(col = "purple")

##Why 2 degrees of freedom? Or how are we choosing that?


# (c) Evaluate the model obtained on the test set, and explain the
# results obtained.

pred <- predict(gam.fit, College.test)
error <- mean((College.test$Outstate - pred)^2)
error

tss <- mean((College.test$Outstate - mean(College.test$Outstate))^2)
rss <- 1 - error / tss
rss

data.frame(error, rss)
# (d) For which variables, if any, is there evidence of a non-linear
# relationship with the response?
summary(gam.fit)


##11 ##Doesn't work
set.seed(2)

x1 <- runif(100)
x2 <- runif(100)
beta1 = 10
y <- cbind(x1,x2)



for (i in 1:10){
  a <- y - beta1*x1
  beta2 <- lm(a ~ x2)$coef[2]
  print(head(beta2))
}

a <- y - beta2*x2
beta1 <- lm(a ~ x1)$coef[2]

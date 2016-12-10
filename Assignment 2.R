## Assignment 2

##10

library(ISLR)
data("Carseats")
head(Carseats)
summary(Carseats)
attach(Carseats)

##a
lm.fit <- lm(Sales ~ Price + Urban + US,data=Carseats)
summary (lm.fit)


##d
lm.fit1 <- lm(Sales ~ Price + US,data=Carseats)
summary(lm.fit1)

##g
confint(lm.fit1, level = 0.95)

##h
par(mfrow =c(2,2))
plot(lm.fit1)

###15
library(ISLR)
library(MASS)
data("Boston")
head(Boston)
summary(Boston)
attach(Boston)

##a

lm.fit.zn=lm(crim~zn,data=Boston)
summary(lm.fit.zn)

lm.fit.indus=lm(crim~indus,data=Boston)
summary(lm.fit.indus)

lm.fit.chas=lm(crim~chas,data=Boston)
summary(lm.fit.chas)

lm.fit.nox=lm(crim~nox,data=Boston)
summary(lm.fit.nox)

lm.fit.rm=lm(crim~rm,data=Boston)
summary(lm.fit.rm)

lm.fit.age=lm(crim~age,data=Boston)
summary(lm.fit.age)

lm.fit.dis=lm(crim~dis,data=Boston)
summary(lm.fit.dis)

lm.fit.rad=lm(crim~rad,data=Boston)
summary(lm.fit.rad)

lm.fit.tax=lm(crim~tax,data=Boston)
summary(lm.fit.tax)

lm.fit.ptratio=lm(crim~ptratio,data=Boston)
summary(lm.fit.ptratio)

lm.fit.black=lm(crim~black,data=Boston)
summary(lm.fit.black)

lm.fit.lstat=lm(crim~lstat,data=Boston)
summary(lm.fit.lstat)

lm.fit.medv=lm(crim~medv,data=Boston)
summary(lm.fit.medv)

plot(chas, crim)
abline(lm.fit.chas, col="red",lwd=3)

##b
lm.all <- lm(crim ~. , data = Boston)
summary(lm.all)


##c
uni.coef <- c(coefficients(lm.fit.zn)[2],
              coefficients(lm.fit.indus)[2],
              coefficients(lm.fit.chas)[2],
              coefficients(lm.fit.nox)[2],
              coefficients(lm.fit.rm)[2],
              coefficients(lm.fit.age)[2],
              coefficients(lm.fit.dis)[2],
              coefficients(lm.fit.rad)[2],
              coefficients(lm.fit.tax)[2],
              coefficients(lm.fit.ptratio)[2],
              coefficients(lm.fit.black)[2],
              coefficients(lm.fit.lstat)[2],
              coefficients(lm.fit.medv)[2])
multi.coef <- coefficients(lm.all)[-1] #discard intercept

plot(uni.coef,multi.coef, col = "red", main = "Univariate vs Multiple Regression Coefficients")

m <-cor(Boston)
corrplot.mixed(m)

###d


lm.fit.zn=lm(crim~poly(zn,3),data=Boston)
summary(lm.fit.zn)

lm.fit.indus=lm(crim~poly(indus,3),data=Boston)
summary(lm.fit.indus)

lm.fit.chas=lm(crim~poly(chas,2),data=Boston)
summary(lm.fit.chas)

lm.fit.nox=lm(crim~poly(nox,3),data=Boston)
summary(lm.fit.nox)

lm.fit.rm=lm(crim~poly(rm,3),data=Boston)
summary(lm.fit.rm)

lm.fit.age=lm(crim~poly(age,3),data=Boston)
summary(lm.fit.age)

lm.fit.dis=lm(crim~poly(dis,3),data=Boston)
summary(lm.fit.dis)

lm.fit.rad=lm(crim~poly(rad,3),data=Boston)
summary(lm.fit.rad)

lm.fit.tax=lm(crim~poly(tax,3),data=Boston)
summary(lm.fit.tax)

lm.fit.ptratio=lm(crim~poly(ptratio,3),data=Boston)
summary(lm.fit.ptratio)

lm.fit.black=lm(crim~poly(black,3),data=Boston)
summary(lm.fit.black)

lm.fit.lstat=lm(crim~poly(lstat,3),data=Boston)
summary(lm.fit.lstat)

lm.fit.medv=lm(crim~poly(medv,3),data=Boston)
summary(lm.fit.medv)


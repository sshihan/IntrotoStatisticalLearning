###10
data("Weekly")
attach(Weekly)
w <- Weekly
head(Weekly)

##1 = Down, 2 = Up
w$Direction <- as.numeric(w$Direction)
head(w)
str(w)
##a. 
summary(Weekly)
pairs(w)

a <- cor(w)
corrplot.mixed(a)
cor(w)

plot(Volume)

##b
glm.fit=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
            data=Weekly ,family =binomial)
summary(glm.fit)

###c
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]
contrasts(Direction)
glm.pred <- rep("Down", 1089)
glm.pred[glm.probs > 0.5] = "Up"

table(glm.pred, Direction)

(54+557)/1089
mean(glm.pred==Direction)

##d
train <- (Year<2009)
head(train)
Weekly.2008 = Weekly[!train,]
head(Weekly.2008)

dim(Weekly.2008)
Direction.2008 <- Direction[!train]
Direction.2008

glm.fit2 <- glm(Direction ~ Lag2, data = Weekly, subset = train, family = binomial)
glm.probs2 <- predict(glm.fit2, Weekly.2008, type = "response")

glm.probs2
summary(glm.fit2)

glm.pred2 <- rep("Down", 104)
glm.pred2[glm.probs2>0.5]="Up"
table(glm.pred2, Direction.2008)

glm <- mean(glm.pred2 == Direction.2008)

(56+9)/104

###e. 
ldafit <- lda(Direction ~ Lag1:Lag5 + Volume, data = Weekly, subset = train)
ldafit

lda.pred <- predict(ldafit, Weekly.2008)
table(lda.pred$class, Direction.2008)
lda <- mean(lda.pred$class==Direction.2008)

###f
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.fit
qda.pred <- predict(qda.fit, Weekly.2008)
summary(qda.pred)
table(qda.pred$class, Direction.2008)
qda <- mean(qda.pred$class==Direction.2008)
qda
###g
train.X <- as.matrix((Lag2)[train])
head(train.X)
test.X <- cbind((Lag2)[!train])
head(test.X)
dim(train.X)
train.Direction <- Direction[train]
head(train.Direction)

test.Direction <- Direction[!train]
head(test.Direction)
dim(train.Direction)
set.seed(8)
knn.pred <- knn(train.X, test.X, train.Direction, k=100)
table(knn.pred, Direction.2008)
knn <- mean(knn.pred != Direction.2008)
(21+31)/(21+22+30+31)


###running Mclust
weeklyMclustDA <- MclustDA(train.X, train.Direction, modelNames = "V")
summary(weeklyMclustDA, parameters = TRUE)
summary(weeklyMclustDA, newdata = test.X, newclass = test.Direction)

weeklyMclustDA <- MclustDA(train.X, train.Direction)
summary(weeklyMclustDA, parameters = TRUE)
Mclustda <- summary(weeklyMclustDA, newdata = test.X, newclass = test.Direction)
mclustda <- Mclustda$err.newdata


TP <- 52/(9+5+38+52)
TP

FP <- 5/(38+9+52+5) 
FP
t <- data.frame(cbind(TP,FP))
t

##b
weeklyMclustDA <- MclustDA(train.X, train.Direction,modelType = "EDDA")
summary(weeklyMclustDA, parameters = TRUE)
Mclustedda <- summary(weeklyMclustDA, newdata = test.X, newclass = test.Direction)
Mclustedda
mclustedda <- Mclustedda$err.newdata

TP1 <- 56/(9+34+5+56)
FP1 <- 9/(9+34+56+5)
t3 <- data.frame(cbind(TP1,FP1))
t3
##c
t2 <- data.frame(cbind(glm,lda,qda,knn,mclustedda,mclustda))
t2


###11

#a
data("Auto")
head(Auto)
attach(Auto)
summary(Auto)

## 0 = less than median
mpg01 <- as.numeric(mpg >= 22.75)
mpg01
A <- data.frame(Auto,mpg01)
head(A)

##b
pairs(A)
a <- cor(A[,-9])
corrplot.mixed(a)

par(mfrow =c(2,2))

boxplot(displacement ~ mpg01, data = A, col = "bisque", main = "MPG01 vs. Displacement")
boxplot(horsepower ~ mpg01, data = A, col = "bisque", main = "MPG01 vs. Horsepower")
boxplot(weight ~ mpg01, data = A, col = "bisque", main = "MPG01 vs. Weight")
boxplot(acceleration ~ mpg01, data = A, col = "bisque", main = "MPG01 vs. Acceleration")

##c splitting the data set 25/75

set.seed(123)
r <- rnorm(nrow(A))
test <- r > quantile(r,0.25)
train <- !test
A.train <- A[train,]
A.test <- A[test,]

###f

glm.fit3 <- glm(mpg01 ~ displacement + weight + acceleration, family=binomial, data=A.train)
summary(glm.fit3)

glm.probs3 <- predict(glm.fit3, A.test, type="response")
glm.pred3 <- rep(0,nrow(A.test))
glm.pred3[glm.probs3 > 0.50] <- 1
table(glm.pred3, A.test$mpg01)
glm <- mean(glm.pred3 != A.test$mpg01)

dim(A.test)
##d.
###d
lda.fit2 <- lda(mpg01 ~ displacement + weight + acceleration, data = A.train)

lda.pred2 <- predict(lda.fit2, A.test)
summary(lda.pred2)
table(lda.pred2$class, A.test$mpg01)
lda <- mean(lda.pred2$class !=A.test$mpg01)
lda
###f
qda.fit2 <- qda(mpg01 ~ displacement + weight + acceleration, data = A.train)
qda.fit2
qda.pred2 <- predict(qda.fit2, A.test)
summary(qda.pred2)
table(qda.pred2$class, A.test$mpg01)
qda <- mean(qda.pred2$class !=A.test$mpg01)

###g
train.X2 <- cbind(weight,horsepower,acceleration)[train,]
test.X2 <- cbind(weight,horsepower,acceleration)[!train,]
dim(train.X2)
dim(test.X2)
train.mpg01 <- mpg01[train]
train.mpg01
test.mpg01 <- mpg01[!train]
set.seed(8)
knn.pred2 <- knn(train.X2, test.X2, train.mpg01, k=15)
table(knn.pred2, test.mpg01)
knn <- mean(knn.pred2 != test.mpg01)

##Mclust
autoMclustDA <- MclustDA(train.X2, train.mpg01,modelType = "MclustDA")
summary(autoMclustDA, parameters = TRUE)
autoda <- summary(autoMclustDA, newdata = test.X2, newclass = test.mpg01)
autoda
automclustda <- autoda$err.newdata
automclustda

TP2 <- 112/(138+11+33+112)
FP2 <- 138/(112+11+33+138)
data.frame(cbind(TP2,FP2))

autoMclustDA <- MclustDA(train.X2, train.mpg01)
summary(autoMclustDA, parameters = TRUE)
summary(autoMclustDA, newdata = test.X2, newclass = test.mpg01)

##b
autoMclustDA2 <- MclustDA(train.X2, train.mpg01,modelType = "EDDA")
summary(autoMclustDA, parameters = TRUE)
autoedda <- summary(autoMclustDA, newdata = test.X2, newclass = test.mpg01)
edda <- autoedda$err.newdata
edda

TP3 <- 130/(260+15+19)
FP3 <- TP3
data.frame(cbind(TP3,FP3))


autoMclustDA <- MclustDA(train.X2, train.mpg01,modelType = "EDDA", modelNames = "EVV")
summary(autoMclustDA, parameters = TRUE)
EVVsum <- summary(autoMclustDA, newdata = test.X2, newclass = test.mpg01)
EVV <- EVVsum$err.newdata
EVV

TP3 <- 130/(260+15+19)
FP3 <- TP3
data.frame(cbind(TP3,FP3))



TP4 <- 134/(131+18+11+134)
FP4 <- 131/(131+18+11+134)
data.frame(cbind(TP4,FP4))


data.frame(cbind(glm,lda,qda,knn,automclustda,edda,EVV))

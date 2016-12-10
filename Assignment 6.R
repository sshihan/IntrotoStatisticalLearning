data("Default")
attach(Default)
head(Default)
str(Default)

##a
set.seed(111)
glm1 <- glm(default~ income+balance, data = Default, family = "binomial")
summary(glm1)

##b
#i 
set.seed(222)
train <- sample(nrow(Default),nrow(Default)-nrow(Default)/3)
d.train <- Default[train,]
d.test <- Default[-train,]
glm2 <- glm(default ~ income + balance,data=d.train, family="binomial")
summary(glm2)
glm2probs <- predict(glm2,d.test,type="response")
glm2pred <- rep("No", length(glm2probs))
glm2pred[glm2probs > 0.5] <- "Yes"
mean(glm2pred!=d.test$default)

##c
set.seed(123)
train <- sample(nrow(Default),nrow(Default)-nrow(Default)/3)
d.train <- Default[train,]
d.test <- Default[-train,]
glm2 <- glm(default ~ income + balance,data=d.train, family="binomial")
summary(glm2)
glm2probs <- predict(glm2,d.test,type="response")
glm2pred <- rep("No", length(glm2probs))
glm2pred[glm2probs > 0.5] <- "Yes"
mean(glm2pred!=d.test$default)

set.seed(213)
train <- sample(nrow(Default),nrow(Default)-nrow(Default)/3)
d.train <- Default[train,]
d.test <- Default[-train,]
glm2 <- glm(default ~ income + balance,data=d.train, family="binomial")
summary(glm2)
glm2probs <- predict(glm2,d.test,type="response")
glm2pred <- rep("No", length(glm2probs))
glm2pred[glm2probs > 0.5] <- "Yes"
mean(glm2pred!=d.test$default)

set.seed(312)
train <- sample(nrow(Default),nrow(Default)-nrow(Default)/3)
d.train <- Default[train,]
d.test <- Default[-train,]
glm2 <- glm(default ~ income + balance,data=d.train, family="binomial")
summary(glm2)
glm2probs <- predict(glm2,d.test,type="response")
glm2pred <- rep("No", length(glm2probs))
glm2pred[glm2probs > 0.5] <- "Yes"
mean(glm2pred!=d.test$default)


##d
set.seed(456)
train <- sample(nrow(Default),nrow(Default)-nrow(Default)/3)
d.train <- Default[train,]
d.test <- Default[-train,]
glm2 <- glm(default ~ income + balance + student,data=d.train, family="binomial")
summary(glm2)
glm2probs <- predict(glm2,d.test,type="response")
glm2pred <- rep("No", length(glm2probs))
glm2pred[glm2probs > 0.5] <- "Yes"
mean(glm2pred!=d.test$default)

###5.4.7

##a
data(Weekly)
attach(Weekly)
W <- Weekly

glm.W <- glm(Direction ~ Lag1 + Lag2, data = W, family = "binomial")
summary(glm.W)

glm.W2 <- glm(Direction ~ Lag1 + Lag2, data = W[-1, ], family = "binomial")
summary(glm.W2)

predict.glm(glm.W2, Weekly[1, ], type = "response") > 0.5
head(W)

set.seed(3333)
error <- rep(0, dim(W)[1])
for (i in 1:dim(W)[1]) {
  glm.W3 <- glm(Direction ~ Lag1 + Lag2, data = W[-i, ],  family = "binomial")
  pred <- predict.glm(glm.W3, W[i, ], type = "response") > 0.5
  true <- W[i, ]$Direction == "Up"
  if (pred != true)
    error[i] <- 1
}
error
mean(error)


###4
data("Auto")
attach(Auto)
head(Auto)
mpg01 <- as.numeric(mpg >= 22.75)
mpg01
A <- data.frame(Auto,mpg01)
head(A)
dim(Auto)
set.seed (17)
cv.error.6 <- rep (0,6)
for (i in 1:6) {
  glm.fit = glm(mpg01~ horsepower+poly(horsepower,i),data=A)
  cv.error.6[i]=cv.glm (A ,glm.fit ,K=6)$delta[1]
}
cv.error.6


###5
head(student.mat)
attach(student.mat)
mean(G1)
mean(G2)
summary(student.mat)

G1_dummy <- as.numeric(G1 >= 11)
G2_dummy <- as.numeric(G2 >= 11)
G3_dummy <- as.numeric(G3 >= 11)

S <- data.frame(student.mat,G1_dummy,G2_dummy,G3_dummy)
head(S)


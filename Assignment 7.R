data("Default")
D <- Default
set.seed(702)
attach(D)
##a
summary(Default)
D_glm <- glm(default~balance+income, data = D, family = "binomial")
summary(D_glm)

##b
boot_fn <- function(D, index) {
  D_glm1 <- glm(default ~ income + balance, data = D, family = "binomial", subset = index)
  return (coef(D_glm1))
}

###c
boot(D, boot_fn, 1000)


##9.
data("Boston")
attach(Boston)

##a
mu <- mean(medv)
mu

##b
se <- sd(medv)/sqrt(dim(Boston)[1])
se

##c
set.seed(702)
boot_f2 <- function(Boston, index) {
  mu1 <- mean(Boston[index])
  return (mu1)
}
boot(medv, boot_f2, 1000)

##d.
t.test(medv)
Confidence_interval <- c(22.53 - 2 * 0.4119, 22.53 + 2 * 0.4119)
Confidence_interval

##e
med <- median(medv)
med

##f
boot_f3 <- function(Boston, index) {
  mu3 <- median(Boston[index])
  return (mu3)
}
boot(medv, boot_f3, 1000)

##g
perc <- quantile(medv,c(0.1))
perc

##h
boot_f4 <- function(Boston, index) {
  mu4 <- quantile(Boston[index], c(0.1))
  return (mu4)
}
boot(medv, boot_f4, 1000)

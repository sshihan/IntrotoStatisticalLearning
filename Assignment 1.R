##2.4.8

write.csv(College, file = "~/Desktop/College.csv")
library(ISLR)
data("College")
college <- College
head(college)
#i. 
summary(college)
attach(college)
dim(college)
str(college)

college =college [,-1]
fix(college)
head(college)

#ii.
m <-cor(college[,1:10])
corrplot.mixed(m)
pairs(college[,1:10])

#iii.
boxplot(College$Outstate ~ College$Private, col = c("red", "yellow"), main = "Outstate versus Private", 
        xlab = "Private", ylab = "Outstate")

#iv. 
Elite <- rep ("No",nrow(college))
Elite [college$Top10perc >50] <- " Yes"
Elite <- as.factor(Elite)
college <- data.frame(college ,Elite)

summary(Elite)
boxplot(Outstate ~ Elite, col = c("purple", "orange"), main = "Outstate versus Elite", 
        xlab = "Elite", ylab = "Outstate")

##v. 
par(mfrow = c(3,2))

hist(college$Room.Board, breaks = 5, freq = TRUE, col = "orange", main = "Histogram of Room & Board", 
     xlab = "Room & Board", ylab = "Value")
hist(college$Room.Board, breaks = 12, freq = TRUE, col = "purple", main = "Histogram", 
     xlab = "Room & Board", ylab = "Value")
hist(college$F.Undergrad, breaks = 6, freq = TRUE, col = "orange", main = "Histogram", 
     xlab = "F.Undergrad", ylab = "Value")
hist(college$F.Undergrad, breaks = 10, freq = TRUE, col = "purple", main = "Histogram", 
     xlab = "F.Undergrad", ylab = "Value")
hist(college$Personal, breaks = 6, freq = TRUE, col = "orange", main = "Histogram", 
     xlab = "Personal", ylab = "Value")
hist(college$Personal, breaks = 10, freq = TRUE, col = "purple", main = "Histogram", 
     xlab = "Personal", ylab = "Value")


#vi.
Acceptancerate<- c(College$Accept/College$Apps)
College$Acceptancerate <- Acceptancerate
head(College)

###Elite schools have slightly higher acceptance rate and higher graduation rate

boxplot(College$Acceptancerate ~ College$Private, col = c("gold", "purple"), main = "Acceptance rate in Private and Public schools", 
        xlab = "Private", ylab = "Acceptance rate")

boxplot(College$Grad.Rate ~ College$Private, col = c("yellow", "purple"), main = "Graduation Rate in Private schools and Public schools", 
        xlab = "Private", ylab = "Graduation rate")


##Number of PhD faculty affecting graduation rate
plot(PhD, Grad.Rate, main = "Number of PhD faculty members with respect to Graduation Rate")

summary(lm(Grad.Rate ~ PhD))
abline(lm(Grad.Rate ~ PhD), col="red") # regression line (y~x) 
lines(lowess(PhD, Grad.Rate), col="blue") # lowess line (x,y)

##Instructional expenditure per student affecting graduation rate
plot(Expend, Grad.Rate, main = "Instructional expenditure per student with respect to graduation rate")

abline(lm(Grad.Rate ~ Expend), col="red") # regression line (y~x) 
lines(lowess(Expend, Grad.Rate), col="blue") # lowess line (x,y)

summary(Expend)

plot(Acceptancerate, Grad.Rate, main = "Acceptance rate with respect to Graduation rate")
abline(lm(Grad.Rate ~ Acceptancerate), col="red") # regression line (y~x) 
lines(lowess(Acceptancerate, Grad.Rate), col="blue") # lowess line (x,y)


library(Rcmdr)
scatter3d(Grad.Rate, Expend, PhD)

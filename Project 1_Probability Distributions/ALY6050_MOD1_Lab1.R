
# ALY-6050 - MOD 1_Lab 1
# Part 1

N1 <- 10000
r1 <- runif(N)
x1 <- -log(r1)
hist(x1, freq=F)
lines(density(x1), lwd=1.5, col='red')
summary(x1)
sd(x1)
library(e1071)
skewness(x1)

N2 <- 10000
r2 <- runif(N)
x2 <- -log(r2)
hist(x2, freq=F)
lines(density(x2), lwd=1.5, col='red')
summary(x2)
sd(x2)
library(e1071)
skewness(x2)

N3 <- 10000
r3<- runif(N3)
x3 <- -log(r3)
hist(x3, freq=F)
lines(density(x3), lwd=1.5, col='red')
summary(x3)
sd(x3)
library(e1071)
skewness(x3)


# Theory: If R is standard uniform, then X=-m*Ln(R) is an 
#         exponential random variable with a mean of m. 

# We suspect X to have an exponential distribution with a mean 
# estimated from the sample:

# We can compare X with the Exponential random variable Y created 
#   with a same value of the mean.

Emp_mean1 <- mean(x1)
y1 <- rexp(N1, rate = 1/Emp_mean1)

qqplot(x1,y1)
abline(0,1,lwd=2,col="red") 

Emp_mean2 <- mean(x2)
y2 <- rexp(N2, rate = 1/Emp_mean2)

qqplot(x2,y2)
abline(0,1,lwd=2,col="red")

Emp_mean3 <- mean(x3)
y3 <- rexp(N3, rate = 1/Emp_mean3)

qqplot(x3,y3)
abline(0,1,lwd=2,col="red")


# Part 2:

#HINT: 
# Note that X=-Ln(r1*r2*r3) is algebraicly equal to:
#   X = - Ln(r1) - Ln(r2) - Ln(r3)
# From Part 1, we know that each term is an exponential random
# variable with an average of 1. 
# Therefore, X is the sum of 3 identical and independent Exponential
#   random variables; each with a mean of 3. 

#  Theory: If X1, X2, ... Xn are n independent and identical 
#   random variables; each having an Exponential probability 
#   distribution with a mean equal to m, then their sum will have
#   the Gamma distribution with parameters m and n; That is, Gamma(m, n)

x <- -log(r1*r2*r3)
hist(x, freq=F)
lines(density(x), lwd=1.5, col='red')
summary(x)
sd(x)
library(e1071)
skewness(x)


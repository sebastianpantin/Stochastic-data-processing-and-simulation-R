# Assignment 1.1

#install.packages('nortest')
library(nortest)
a = 2
b = 2
n = 100
gammaDist = rgamma(100, shape = a, scale = b)
p1 <- hist(gammaDist,breaks=15)
qqnorm(gammaDist)
qqline(gammaDist)
lillie.test(gammaDist)
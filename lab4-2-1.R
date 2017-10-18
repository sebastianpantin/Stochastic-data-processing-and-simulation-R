n <- 100;
k <- 2;
N <- 1000;
gamma <- 2;
gammaDist <- rgamma(n, shape = k, scale = gamma)

means <- vector()
vars <- vector()
sampledData <- vector()
for (i in 1:1000) {
  sampledData <- sample(gammaDist,size=n, replace = TRUE) #sample the data with replacement
  means <- c(means, mean(sampledData)) # Means of the sampled data
}
bias <- mean(means)-mean(gammaDist) #Estimated bias
variance <- var(means) #Estimated variance
print(bias)
print(variance)
hist(means,breaks=20)
lines(density(means))
abline(v=k*gamma, col="red") # The red line indicates the actual mean.
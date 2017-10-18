n <- 100;
k <- 2;
gamma <- 2;
gammaDist <- rgamma(n, shape = k, scale = gamma);
sampleData <- vector()

LL <- function(param,X){
  kLL <- param[1]
  gammaLL <- param[2]
  -sum(dgamma(X,kLL,scale=gammaLL,log=TRUE)) #the log likelihood function = log of probability density function
}
estimatedParams <- nlm(LL,c(1,1), X=gammaDist)$estimate  #Estimated parameters,k and gamma
estimatedGammaDist <- rgamma(n, shape = estimatedParams[1], scale = estimatedParams[2]) #make new distribution from estimated params
means <- vector()
sampledDataFromEstimatedGammaDist <- vector()
for (i in 1:1000) {
  sampledDataFromEstimatedGammaDist <- sample(estimatedGammaDist,size=n, replace = TRUE)
  means <- c(means, mean(sampledDataFromEstimatedGammaDist)) #calculate the means
}
bias <- mean(means)-mean(estimatedGammaDist) #Estimated bias
variance <- var(means) #Estimated variance
print(bias)
print(variance)
hist(means,15)
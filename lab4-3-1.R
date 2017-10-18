n <- 100;
k <- 2;
gamma <- 2;
gammaDist <- rgamma(n, shape = k, scale = gamma); #Gamma distribution
sampleData <- vector()

LL <- function(param,X){
  kLL <- param[1] 
  gammaLL <- param[2]
  -sum(dgamma(X,kLL,scale=gammaLL,log=TRUE)) #the log likelihood function = log of probability density function
}
sampleData <- sample(gammaDist,size=n, replace = TRUE) # new sample from data
nlm(LL,c(1,1), X=sampleData) #Maximize log likelihood function, it numerically differentiate

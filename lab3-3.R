#install.packages('alabama')
library(alabama)

expectedLifeLength <- function(x){ #function that calculates the expected life length
  SurvivalFunction <- function(time){
    b = 1/3;
    R <- (1-(1-exp(-(x[1]*time)^b))^2)*(exp(-(x[2]*time)^b)); #x[1] = lambda, x[2] = mu
    return(R)
  }
  expectedLifeLength = -1*integrate(SurvivalFunction,0,Inf)$value #-1 to maximize instead of minimize
  return(expectedLifeLength)
}

startValue <- function(cost){ # calculates the starting value
  mu =4;
  lambda = 2/(cost-3/5-1/mu); #constraints for the starting value of the parameters
  return(c(lambda,mu))
}

estimatedParameters <- vector()
estimatedExpectedLifeLength <- vector()
for (cost in 1:10){ #loop for all costs
  p0 = startValue(cost);
  heq <- function(x){
    h <- rep(NA, 1)
    h[1] <- 3/5+2/x[1]+1/x[2] - cost #The total cost as a constrain, should equal zero because of constrOptim
    h
  }
  hin <- function(x){
    h <- rep(NA, 2)
    h[1] <- x[1] #Both parameters must be positive because of exp
    h[2] <- x[2]
    h
  }
  estimatedParameters <- c(estimatedParameters,constrOptim.nl(par=p0, fn=expectedLifeLength, heq=heq, hin=hin)$par)
  
  
}
parametersMatrix <- matrix(estimatedParameters,10,2,byrow = TRUE)
estimatedExpectedLifeLength <- apply(parametersMatrix,1,expectedLifeLength)*-1
plot(c(1:10), parametersMatrix[,1], type = 'p',ylim=c(0,10), ylab="Estimated parameters", col="red", xlab="cost")
par(new=TRUE)
plot(c(1:10), parametersMatrix[,2], type = 'p',ylim=c(0,10), ylab="Estimated parameters", col="blue", xlab="cost")
legend(8,5, c("lambda", "mu"),col = c(2,4),
       text.col = "green4", lty = 0,pch = 1, bg = "gray90")
plot(c(1:10), estimatedExpectedLifeLength, type = 'p',ylim=c(0,max(estimatedExpectedLifeLength)), ylab="Estimated life length", col="blue", xlab="cost")

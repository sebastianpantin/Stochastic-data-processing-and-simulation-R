n <- 100;
k <- 2;
N <- 1000;
gamma <- 2;
gammaDist <- rgamma(n, shape = k, scale = gamma); #Gamma distribution

meanFun = function(data){ #function to calculate mean for boot
  return(mean(data))  
}

MLE <- function(X){ #Maximum likelihood estimation
  LL <- function(param,X){ #Log likelihood function
    kLL <- param[1]
    gammaLL <- param[2]
    if(kLL <= 0 || gammaLL <= 0){ #we only want positive numbers for the exp fun
      return(1e200) 
    } else {
      return(-sum(dgamma(X,kLL,scale=gammaLL,log=TRUE)))
    }
  }
  estimatedParam = nlm(LL,c(1,1), X=X)$estimate #Maximizes the Log likelihood function
  return(estimatedParam)
}

gamma.rg <- function(data,mle){
  return(rgamma(n, shape=mle[1], scale=mle[2])) #Generate random gamma from the estimated params
}

mle <- MLE(gammaDist) #The estimated parameters

output = boot(data=gammaDist, statistic=meanFun, R=N, sim="parametric", ran.gen = gamma.rg, mle=mle)
plot(output)
#original:
output$t0
#bias:
mean(output$t)-output$t0
#var: 
sd(output$t)^2


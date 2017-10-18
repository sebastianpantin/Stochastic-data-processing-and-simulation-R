n<-seq(10,100,10)
k <- 2;
N <- 1000;
gamma <- 2;
trueMean <- 4;

meanFun = function(data){  #function to calculate mean for boot
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

nbrOfIsWithin <- vector()
for (j in n){ #for j=10,20,...,100
  print(j)
  gamma.rg <- function(data,mle){
    return(rgamma(j, shape=mle[1], scale=mle[2])) #Generate random gamma from the estimated params
  }
  isWithinCi = 0;
  
  for (i in 1:N){
    gammaDist <- rgamma(j, shape = k, scale = gamma) #Gamma distribution
    mle <- MLE(gammaDist) #The estimated parameters
    output = boot(data=gammaDist, statistic=meanFun, R=N, sim="parametric", ran.gen = gamma.rg, mle=mle)
    CIData = boot.ci(output, conf = 0.95, type="norm")$normal
    bootCILow <- CIData[[1,2]]
    bootCIHigh <- CIData[[1,3]]
    
    if(trueMean > bootCILow && trueMean < bootCIHigh){ #Check if CI covers the true value of mean
      isWithinCi <- isWithinCi + 1;
    }
  }
  nbrOfIsWithin <- c(nbrOfIsWithin, isWithinCi)
  print(nbrOfIsWithin[j/10])
}
plot(n, nbrOfIsWithin, type="l")


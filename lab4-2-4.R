library(boot) #load the package
ptm <- proc.time() #Take time
n<-seq(10,100,10)
k <- 2;
N <- 1000;
gamma <- 2;
trueMean <- 4;
nbrOfIsWithin <- vector()
meanFun = function(data,index,formula){  #function to calculate the mean for boot
  d = data[index]
  return(mean(d))  
}

for (j in n){ #For j=10,20,...,100
  isWithinCi = 0;
  for (i in 1:N){
    gammaDist <- rgamma(j, shape = k, scale = gamma)
    output = boot(data=gammaDist, statistic=meanFun, R=N)
    CIData = boot.ci(output, conf = 0.95, type="norm")$normal
    bootCILow <- CIData[[1,2]]
    bootCIHigh <- CIData[[1,3]]
    
    if(trueMean > bootCILow && trueMean < bootCIHigh){ #Check if CI covers the true value of mean
      isWithinCi <- isWithinCi + 1;
    }
  }
  nbrOfIsWithin <- c(nbrOfIsWithin, isWithinCi)
}

plot(n, nbrOfIsWithin, type="l")
proc.time() - ptm #Stop time

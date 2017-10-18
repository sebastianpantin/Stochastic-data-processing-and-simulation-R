library(boot) #load the package
ptm <- proc.time() #start time
n<-seq(10,100,10)
k <- 2;
N <- 1000;
gamma <- 2;
trueVar <- 8;
nbrOfIsWithin <- vector()
varFun = function(data,index,formula){ #function to calculate variance for boot 
  d = data[index]
  return(var(d))  
}

for (j in n){ #for j=10,20,...,100
  print(j)
  isWithinCi = 0;
  for (i in 1:N){
    gammaDist <- rgamma(j, shape = k, scale = gamma) #Gamma distribution
    output = boot(data=gammaDist, statistic=varFun, R=N)
    CIData = boot.ci(output, conf = 0.95, type="basic")$basic
    bootCILow <- CIData[[1,4]]
    bootCIHigh <- CIData[[1,5]]
    
    if(trueVar > bootCILow && trueVar < bootCIHigh){ #Check if CI covers the true value of variance
      isWithinCi <- isWithinCi + 1;
    }
  }
  nbrOfIsWithin <- c(nbrOfIsWithin, isWithinCi)
  print(nbrOfIsWithin[j/10])
}

plot(n, nbrOfIsWithin, type="l")
proc.time() - ptm #stop time

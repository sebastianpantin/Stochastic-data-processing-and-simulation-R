library(boot)
library(tictoc)
boot.var <- function(data,index){ #vectorized version of boot that calculates var and re-samples
  boot.sample <- data[index] # bootstrapped sample
  m <- var(boot.sample)       # bootstrapped var
  n <- length(boot.sample)
  R <- 50
  reboot.samples <- matrix(data=sample(boot.sample,size=R*n,replace=T),nrow=n) #Resample
  reboot.var <- apply(reboot.samples,MARGIN=2,var) #calculate variance
  m.var <- var(reboot.var)
  results <- c(m,m.var)
  return(results)
}

k = 2;
gamma = 2;
trueVar = k*gamma^2

n = seq(10,100,10)
isInIntervalVector <- rep(0,10)
for(j in n){ #for j=10,20,...,100
  isInInterval = 0;
  tic()
  for(i in 1:100){
    X = rgamma(j, shape = k , scale = gamma) #Gamma distribution
    b <- boot(X,boot.var,R=1000)
    CI = boot.ci(b,type="stud")$stud
    lowerBound = CI[[1,4]]
    upperBound = CI[[1,5]]
    if((trueVar > lowerBound)&&(trueVar < upperBound)){ #Check if CI covers the true value of variance
      isInInterval = isInInterval + 1;
    }
  }
  toc()
  isInIntervalVector[j/10] = isInInterval;
  print(isInIntervalVector[j/10])
}

plot(n,isInIntervalVector, type='l')

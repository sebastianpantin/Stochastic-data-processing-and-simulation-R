SurvivalFunctionCold <- function(time){ #Survival function of system with cold redundant component
  a=1;
  b=0.5;
  lambda=0.5;
  R = (1-(1-exp(-(a*time)^b))^3)*(exp(-lambda*time)*(lambda*time+1));
  return(R)
}

DeathRateFunctionCold <- function(time){ #Death rate of system with cold redundant component
  r <- log(SurvivalFunctionCold(time))
}

DeathRateFunctionWarm <- function(time){ #Death rate of system with warm redundant component
  r <- log(SurvivalFunctionWarm(time))
}

SurvivalFunctionWarm <- function(time){ #Survival function of system with warm redundant component
  a=1;
  b=0.5;
  lambda=0.5;
  R = (1-(1-exp(-(a*time)^b))^3)*(1-(1-exp(-lambda*time))^2);
  return(R)
}


expectedLifeLengthCold <- integrate(SurvivalFunctionCold,0,Inf)$value
expectedLifeLengthWarm <- integrate(SurvivalFunctionWarm,0,Inf)$value
expectedLifeLengthCold
expectedLifeLengthWarm
curve(SurvivalFunctionCold,0,10)
curve(SurvivalFunctionWarm,0,10)

time = seq(0.1, 10, length.out =1000)
rcold <- -grad(DeathRateFunctionCold,time)
plot(time, rcold,type="l",xlim=c(0,10),ylim=c(0.3,0.7),ylab="r(t)", col="red")
par(new=TRUE)
rwarm <- -grad(DeathRateFunctionWarm,time)
plot(time, rwarm,type="l",xlim=c(0,10),ylim=c(0.3,0.7), ylab="r(t)", col="blue")
legend(8,0.4, c("rcold(t)", "rwarm(t)"),col = c(2,4),
       text.col = "green4", lty = c(1, 1, 1), bg = "gray90")


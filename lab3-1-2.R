SurvivalFunction <- function(time){ #Surival function of the system
  a=1;
  b=0.5;
  lambda=0.5;
  R = (1-(1-exp(-(a*time)^b))^3)*exp(-lambda*time);
  return(R)
}

DeathRateFunction <- function(time){ # Death rate function of the system
  r <- log(SurvivalFunction(time))
}


expectedLifeLength <- integrate(SurvivalFunction,0,Inf)
curve(SurvivalFunction,0,10) # Used instead of plot as it takes a function as argument
time = seq(0.1, 10, length.out =1000)
r <- -grad(DeathRateFunction,time) # Derivate of the death rate function
plot(time, r,type="l")

fourthComponentCausesDeath <- 0.5*expectedLifeLength$value #Probability that the fourth component causes death of system
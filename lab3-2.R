SurvivalFunctionCold <- function(time){
  a=1;
  b=0.5;
  lambda=0.5;
  R = (1-(1-exp(-(a*time)^b))^3)*(exp(-lambda*time)*(lambda*time+1));
  return(R)
}

DeathRateFunctionCold <- function(time){
  r <- log(SurvivalFunctionCold(time))
}

DeathRateFunctionWarm <- function(time){
  r <- log(SurvivalFunctionWarm(time))
}

SurvivalFunctionWarm <- function(time){
  a=1;
  b=0.5;
  lambda=0.5;
  R = (1-(1-exp(-(a*time)^b))^3)*(1-(1-exp(-lambda*time))^2);
  return(R)
}


expectedLifeLengthCold <- integrate(SurvivalFunctionCold,0,Inf)
expectedLifeLengthWarm <- integrate(SurvivalFunctionWarm,0,Inf)
expectedLifeLengthCold$value
expectedLifeLengthWarm$value
curve(SurvivalFunctionCold,0,10)
curve(SurvivalFunctionWarm,0,10)

time = seq(0.1, 10, length.out =1000)
rcold <- -grad(DeathRateFunctionCold,time)
plot(time, rcold,type="l")
rwarm <- -grad(DeathRateFunctionWarm,time)
plot(time, rwarm,type="l")
#fourthComponentCausesDeath <- 0.5*expectedLifeLength$value


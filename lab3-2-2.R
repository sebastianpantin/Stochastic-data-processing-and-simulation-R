OptimizedForCold <- function(lambda){ #Optimize rho for system to behave as if with cold redundant
  SurvivalFunctionCold <- function(time){
    a=1;
    b=0.5;
    R = (1-(1-exp(-(a*time)^b))^3)*exp(-lambda*time);
    return(R)
  }
  expectedLifeLength = integrate(SurvivalFunctionCold,0,Inf)$value-expectedLifeLengthCold #ExpectedLifeLengthCold is the integral of surival function of cold redundant
  return(expectedLifeLength)
}

OptimizedForWarm <- function(lambda){ #Optimize rho for system to behave as if with warm redundant
  SurvivalFunctionWarm <- function(time){
    a=1;
    b=0.5;
    R = (1-(1-exp(-(a*time)^b))^3)*exp(-lambda*time);
    return(R)
  }
  expectedLifeLength = integrate(SurvivalFunctionWarm,0,Inf)$value-expectedLifeLengthWarm #ExpectedLifeLengthWarm is the integral of surival function of warm redundant
  return(expectedLifeLength)
}

lambdaToBeLikeCold = uniroot(OptimizedForCold, c(0,0.5), tol=0.00001)$root
lambdaToBeLikeWarm <- uniroot(OptimizedForWarm, c(0,0.5), tol=0.00001)$root
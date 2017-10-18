g <- function(timeVector) {
  nbrOfTimes <- length(timeVector)
  out <- rep(1,nbrOfTimes);
  out[timeVector < 0 ] = 0
  out[timeVector > 0.5 ] = 0
  return(out)
}
n=100;
xsi <- rexp(n, rate = 1)
eta <- rexp(n, rate = 1)

shotNoise <- function(time){
  k1 <- length(xsi[cumsum(xsi) <= 10])
  k2 <- length(eta[cumsum(eta) <=0.5])
  
  x = sum(g(time-cumsum(xsi[cumsum(xsi) <= 10])))+sum(g(time+cumsum(eta[cumsum(eta) <= 0.5])))
  
}

n=100;
time = matrix(seq(0,10,0.1),1)
result = cumsum(c(0,(apply(time, 2, shotNoise))))

plot(stepfun(time,result),xlim = c(0,10), do.points =F)
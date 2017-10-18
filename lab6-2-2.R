g <- function(timeVector) {
  nbrOfTimes <- length(timeVector)
  out <- rep(1,nbrOfTimes);
  out[timeVector < 0 ] = 0
  out[timeVector > 0.5 ] = 0
  return(out)
}

shotNoise <- function(time,xsi,eta){
  k1 <- length(xsi[cumsum(xsi) <= 10])
  k2 <- length(eta[cumsum(eta) <=0.5])
  if(k1<1){
    x = g(time)+sum(g(time+cumsum(eta[cumsum(eta) <= 0.5])))
  }
  else if(k2<1){
    x = g(time)+ sum(g(time-cumsum(xsi[cumsum(xsi) <= 10])))
  }
  else if(k1<1 && k2<1){
    x = 2*g(time);
  }
  else {
    x = sum(g(time-cumsum(xsi[cumsum(xsi) <= 10])))+sum(g(time+cumsum(eta[cumsum(eta) <= 0.5])))
  }
  
}

n=100;
N=1000;
zeta <- rep(0,N)
time = matrix(seq(0,10,0.01),1)
for (i in 1:N){
  xsi <- rexp(n, rate = 1)
  eta <- rexp(n, rate = 1)
  result = apply(time, 2, shotNoise,xsi=xsi,eta=eta)
  if (length(result[result>2])>0){
    zeta[i]=1;
  }
  
}
p <- mean(zeta)
std <- sd(zeta)
lowerBound <- p - qnorm(0.975)*std/sqrt(N)
upperBound <- p - qnorm(0.025)*std/sqrt(N)
CI <- c(lowerBound,upperBound)
print(CI)

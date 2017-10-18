g <- function(timeVector) {
  nbrOfTimes <- length(timeVector)
  out <- rep(1,nbrOfTimes);
  out[timeVector < 0 ] = 0
  out[timeVector > 0.5 ] = 0
  return(out)
}

shotNoise <- function(time,xsi,eta){
  x = sum(g(time-cumsum(xsi[cumsum(xsi) <= 10])))+sum(g(time+cumsum(eta[cumsum(eta) <= 0.5])))
  
}
time = matrix(seq(0,10,0.01),1)
n=100;
N=10000;
zeta = rep(0,N);
for(i in 1:N){
  xsi <- rexp(n, rate = 1)
  eta <- rexp(n, rate = 1)
  
  #result = cumsum(c(0,(apply(time, 2, shotNoise, xsi=xsi, eta=eta))))
  
  k1 <- length(xsi[cumsum(xsi) <= 10])
  k2 <- length(eta[cumsum(eta) <= 0.5])
  M <- vector();
  if(k2>=0 && k1>= 0){
  M <- c(-cumsum(eta[1:k2]),cumsum(xsi[1:k1]))
  } else {
    M<-0;
  }
  M <- sort(M)
  M <- M[-0.5 <= M]
  if(length(M)>2){
    for(h in 3:length(M)){
        if(M[h]-M[h-2]<=0.5){
          zeta[i] = 1;
          break
        }
    }
  }
}
sum(zeta)
p <- mean(zeta)
std <- sd(zeta)
lowerBound <- p - qnorm(0.975)*std/sqrt(N)
upperBound <- p - qnorm(0.025)*std/sqrt(N)
CI <- c(lowerBound,upperBound)
print(CI)





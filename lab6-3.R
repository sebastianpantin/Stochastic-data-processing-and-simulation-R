f <- function(timeVector) {
  out = 1-timeVector^2;
  out[abs(timeVector) > 1 ] = 0
  return(out)
}

time=seq(0,10,0.01);
n  = 1000;
x <- seq(0,max(time),by=max(time)/n)

gaussianProcess<- function(t){
  sum=0;
  for(k in (-n^2):n^2){
    sum = sum + f(t+k/n)*rnorm(1,mean =0, sd = sqrt(1/n));
  }
  return(sum)
}

result <- gaussianProcess(x)
plot(x,result, type="l")
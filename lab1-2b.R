sigmaCI<-function(x,alpha){
  S = var(x)
  n = length(x)
  CI = c(((n-1)*S)/(qchisq(1-alpha/2,n-1)), ((n-1)*S)/(qchisq(alpha/2,n-1)))
  return(CI)
}
a = 2
b = 2
N = 1000
alpha = 0.05
isInRange = matrix(0,N,1)
stdVector = a*b^2
intervalWVector = matrix(0,N,1)
gammaDist = rgamma(n = 100,shape = a, scale = b)
for (i in 1:N){
  gammaDist = rgamma(n = 100,shape = a, scale = b)
  CI = sigmaCI(gammaDist, alpha)
  intervalWVector[i] = CI[2]-CI[1];
  if (stdVector> CI[1] && stdVector<CI[2]) {
    isInRange[i] = 1;
  }
}
sum(isInRange)
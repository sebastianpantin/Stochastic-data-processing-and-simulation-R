n = 100;
N = 1000;
meanArray = matrix(0,N,1);
medianArray = matrix(0,N,1);
k=0.1*n;
for (i in 1:N){
  Y = rnorm(n,0,1)
  r=runif(n, 0,1)
  Z = rt(n,1)
  
  eps = 0.05;
  
  X=Y;
  X[r < eps] = Z[r < eps];
  X = sort(X);
  medianArray[i] = median(X);
  meanArray[i] = sum(X[(k+1:n-k)]/(n-2*k));
}
sortedMedianArray = sort(medianArray);
sortedMeanArray = sort(meanArray);
mean25 = sortedMeanArray[25];
mean975 = sortedMeanArray[975];
hist(sortedMeanArray)
hist(sortedMedianArray)
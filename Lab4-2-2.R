n <- 100;
k <- 2;
N <- 1000;
gamma <- 2;
gammaDist <- rgamma(n, shape = k, scale = gamma)
library(boot) #load the package
# Now we need the function we would like to estimate
# In our case the theta/mean:
meanFun = function(data,index,formula){  
  d = data[index]
  return(mean(d))  
}

# 
output = boot(data=gammaDist, statistic=meanFun, R=N)
plot(output)
#original data:
output$t0
#bias:
mean(output$t)-output$t0
#var: 
var(output$t)

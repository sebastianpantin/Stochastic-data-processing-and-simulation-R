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
#original data:
output$t0
#bias:
mean(output$t)-output$t0
#var: 
sd(output$t)^2
#Basic CI
boot.ci(output, conf = 0.95, type="basic")
sortedMeans = sort(output$t)
2*mean(gammaDist)-sortedMeans[975]
2*mean(gammaDist)-sortedMeans[25]
# Normal
boot.ci(output, conf = 0.95, type="norm")
lower = qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE);
higher = qnorm(0.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
se = sd(output$t)
lowerCI = mean(gammaDist)-lower*se
higherCI = mean(gammaDist)-higher*se
print(lowerCI)
print(higherCI)

# percentile
boot.ci(output, conf = 0.95, type="perc")
sortedMeans = sort(output$t)
sortedMeans[25]
sortedMeans[975]

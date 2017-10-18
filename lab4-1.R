n <- 100;
k <- 2;
gamma <- 2;
gammaDist <- rgamma(n, shape = k, scale = gamma) 
hist(gammaDist,20)

means <- vector()
vars <- vector()
for (i in 1:1000) {
  means <- c(means, mean(rgamma(n, shape=k, scale=gamma))) #Calculate the means and save as vector
  vars <- c(vars, var(rgamma(n, shape=k, scale=gamma))) #Calculate the variances and save as vector
}
hist(means,breaks=20)
lines(density(means))
abline(v=k*gamma, col="red") # The red line indicates the actual mean.
mean(means) #theortical var is gamma^2/n
var(means)
qqnorm(means)
qqline(means)
lillie.test(means) #mostly normal distributed

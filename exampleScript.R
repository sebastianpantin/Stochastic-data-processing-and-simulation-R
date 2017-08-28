<-5 #assign value to variable
A=5 #alternative
x <- c(1,2,3,4) #vector assignment (c=concatenate)
y<-1:8 #(vector assignment)
z<-seq(4,11) #vector assignment
rep(4,times=5) #repeat
rep(4,times=A)
c(x,y) #concatenate
y*2
y*z #element-wise
y*x #x is 'helpful'
sample<-rnorm(10) #random normal distr.
test<-matrix(rnorm(30),ncol=3) #create a matrix from a vector
apply(test,2,mean) #apply mean to each column
myfun<-function(x){
  y=2*x+3
  return(y)
}
myfun(5)
apply(test,1,myfun) #apply own function
apply(test,1,function(x) mean(x)-5) #define function directly
y>=6 #
(y>6 & y<=4)
(y>6 & z<=4)
sum(y<6)
y[y>=6]
test[1,1] #sub-setting
test[,2] #all elements in second column
test[,c(2,3)] #all elements in second and third column
lillie.test()
install.packages('nortest')
library(nortest)
lillie.test()
result<-t.test(y,z) #two sample t.test
str(result) #structure
result$p.value #extract p.value
?t.test #HELP
aa<-NULL
for (i in 1:10){
  aa[i]<-3*i
}
bb<-NULL
for (i in 1:10){
  bb<-c(bb,i)
}
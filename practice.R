4+5*7
install.packages('fitdistrplus')
library(fitdistrplus)

log(2^(1/3)+4)
x1 = 2^(1/3)
log(xl+4)
ls()
rm(x1)

Exp = function(lambda, x){
  value = lambda*exp(-lambda*x)
  return(value)
}
Exp(0.5,10)

vctr = c(1,3,5)
vctr[1]

head(iris)
colnames(iris) 
iris$Sepal.Length>5
iris[iris$Sepal.Length>5,]

x = list(1:10, matrix(1:9, nrow=3, ncol=3), iris)
names(x) = c('vector','matrix','list')
names(x)
x[[1]]
x[[2]]  

x[1]
x$vector

if(iris[1,1]>5){
  print("5보다 큼")
} else{
  print("5보다 작아요")
}

x = ifelse(iris[1,1]>5,">5","<5")

x = iris[,1]
which(x>5)

emp = c()
for(i in 1:5){
  emp[i] = runif(1)
}




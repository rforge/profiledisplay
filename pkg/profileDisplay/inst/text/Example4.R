set.seed(20130821)
X <- runif(100000)
Y <- runif(100000)

#Directly add the vectors together
Z <- numeric()
add4 <- function(X,Y){
  Z <- X +Y
  return(Z)
}
add4(X,Y)

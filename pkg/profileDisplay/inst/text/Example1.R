set.seed(20130821)
X <- runif(100000)
Y <- runif(100000)

#Initially test by adding length to a vector
Z <- numeric()
add1 <- function(X,Y){
  for (i in 1:max(length(X),length(Y))) {
    Zold <- X[i] + Y[i]
    Z <- c(Z,Zold)
  }
  return(Z)
}
add1(X,Y)

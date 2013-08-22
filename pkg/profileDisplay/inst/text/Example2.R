set.seed(20130821)
X <- runif(100000)
Y <- runif(100000)

#Remove lengthening vector but test now by adding to a value in the vector Z
Z <- numeric()
add2 <- function(X,Y){
  for (i in 1:max(length(X),length(Y))) {
    Z[i] <- X[i] + Y[i]
  }
  return(Z)
}
add2(X,Y)

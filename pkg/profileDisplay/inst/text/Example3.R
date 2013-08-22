set.seed(20130821)
X <- runif(100000)
Y <- runif(100000)

#Test by adding to a matrix and transforming back to a numeric vector
Z <- numeric()
add3 <- function(X,Y){
  Xm <- t(matrix(X,nrow=length(X)))
  Ym <- t(matrix(Y,nrow=length(Y)))
  Z <- as.numeric(Xm +Ym)
  return(Z)
}
add3(X,Y)

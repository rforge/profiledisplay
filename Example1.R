X<- rnorm(100000)
Y <- rnorm(100000)
Z <- c()
for (i in 1:100000) {
Z <- c(Z, X[i] + Y[i])
}

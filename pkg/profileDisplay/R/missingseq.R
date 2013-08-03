missingseq <- function(x){
  old <- x
  x <- x[order(x)]
  miss <- x[which(diff(x) != 1)]
  if (length(which(diff(x) != 1)) == 0){
    return(0)
  }
  hold <- numeric()
  holder <- numeric()
  for (i in 1:length(miss)){
    hold <- miss[i]:x[which(miss[i] == x)+1]
    hold <- hold[-1]
    hold <- hold[-length(hold)]
    holder <- c(holder,hold)
  }
  return(holder)
}

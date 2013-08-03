linenumbers <- function(htmldata, int=12) {
  alpha <- letters[1:int]
  measure <- numeric()
  for (i in 1:length(alpha)){
  sa <- sub(paste("<span class=\"", alpha[i], "\">.*", sep=""),paste("<span class=\"", alpha[i], "\">", sep=""),htmldata) 
  sa <- sub(paste(".*<span class=\"", alpha[i] ,"\">", sep=""),paste("<span class=\"", alpha[i], "\">", sep=""),sa) 
  num <- which(sa == paste("<span class=\"", alpha[i], "\">", sep=""))
  sb <- sub("</span>.*", "", htmldata[num])
  sb1 <- sub(".*<span class=\"line\">", "", sb)
  sc <- as.numeric(sb1)
  measure[i] <- length(sc)
}
  return(measure)
}
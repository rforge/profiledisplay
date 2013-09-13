barview <- function(int, htmldata) {
  alpha1 <- character()
  bar <- ""
  for (i in 1:int) {
  bar <- c(bar, paste(rep("o",i),collapse=""))
  alpha1[i] <- paste("<span class=\"", letters[i], "\">", sep="")
  }
bar <- format(bar)
  for (k in 1:int) {
  for (j in grep(alpha1[k], htmldata)) {
    htmldata[j] <- paste(bar[k], htmldata[j], collapse="")
  }
  }
  val <- numeric()
  for (l in 1:int){
   moreval <- grep(alpha1[l], htmldata)
   val <- c(val,moreval)
  }
  miss <- missingseq(val)
  for (n in miss){
    htmldata[miss]  <- paste(bar[1], htmldata[miss], collapse="")
  }
return(htmldata)
}

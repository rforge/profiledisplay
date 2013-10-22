summaryHTML <- function(prof){
  htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", 
          HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(prof), sep=""), local=TRUE)
}
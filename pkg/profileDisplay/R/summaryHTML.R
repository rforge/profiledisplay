summaryHTML <- function(prof,show){
  if (show == "memory"|show == "self"|show=="total"){
    if (show == "memory"){
    htmlize(system.file("text/summarymemory.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(prof), sep=""), local=TRUE)
    }
    if (show == "self"|show=="total") {
      htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", 
              HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(prof), sep=""), local=TRUE)
    }
  } else {
    warning("Not implimented yet.")
  }
}

listRfiles<-function(s="Rprof.out"){
  if(is.character(s)){
    s<-summaryRprof(s,lines="show")
  }
  if(!is.list(s)||!("by.line" %in% names(s))){
    stop("s should be a profile object with line profiling")
  }
  loc <- rownames(s$by.line)
  names <- sub("#.*","",loc)
  if(nrow(s$by.line)==0||all(rownames(s$by.line)=="<no location>")||length(names)==0){
    stop("running time less than 0.02s")}
  names <- levels(factor(names))
  test <- c("<no location>", "Tools.R")
  for (i in 1:length(test)){
    if (length(grep(test[i], names)) >0) {
      noloc <- which(names == "<no location>")
      tool <- which(names == "Tools.R")
      names <- names[-c(noloc,tool)]
    }
  }
  return(names)
}
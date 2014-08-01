styleLabel <- function(x){
  breakPoints <- attr(x,"breakPoints")
  ngp <- attr(x,"ngp")
  label <- character()
  for(i in 1:ngp-1){
    label[i] <- paste("(",round(breakPoints[ngp+1-i],2),", ",round(breakPoints[ngp+2-i],2),"]",sep="")
  }
  label[ngp] <- paste("barely takes time")
  return(label)
}


print.lineClassifier<-function(x, detail= FALSE,...){
  files <- x$files
  profileType <- attr(x,"profileType")
  prof <- x$summaryRprof
    
    if(!detail) output <- structure(list(Type =profileType, Classification = files))
    else output <- structure(list(Type =profileType, Classification = files, summaryRprof = prof))
                                         
   print(output,...)
}
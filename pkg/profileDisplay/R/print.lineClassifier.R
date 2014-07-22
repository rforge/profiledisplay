
print.lineClassifier<-function(x, detail="FALSE"){
  files <- x$files
  profileType <- attr(x,"profileType")
  prof <- x$summaryRprof
  
  if(detail == "FALSE") output <- structure(list(Type =profileType, Classification = files))
  if(detail == "TRUE") output <- structure(list(Type =profileType, Classification = files, summaryRprof = prof))

                                         
  return(output)
}
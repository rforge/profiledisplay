selfprofileClassifier <- function(prof="Rprof.out",filename="all",legend=TRUE,colourdata,...) { #how to directly input these results if function is within display profile?  
  s <- summaryRprof(prof, lines="show")
  total.sampling.time <- s$sampling.time
  loc <- rownames(s$by.line)
  fn <- sub("#.*","",loc)
  interval <- s$sample.interval
  names <- levels(factor(fn))
  if (charmatch("all",filename,nomatch=0)>0) {
    filename <- names
  }
  if (charmatch("<no location>",names,nomatch=0)>0) {
    names <- names[-grep("<no location>",names)]
  }
  if (charmatch("<no location>",filename,nomatch=0)>0) {
    filename <- filename[-grep("<no location>",filename)]
  }
  doc <- basename(filename)
  nums <- numeric()
  numbs <- numeric()
  q <- numeric()
  for (i in 1:length(doc)) {
    nums[i] <- grep(doc[i],names)
  }
  for (j in 1:length(names[nums])) {
    numbs  <- grep(doc[j],loc)
    q <- c(q,numbs)
  }
  h <- s$by.line[q,]
  values <- h$self.time
  loc <- rownames(h)
  fn <- sub("#.*","",loc)
  ln <- sub(".*#","",loc)
  names <- levels(factor(fn))
  name <- unlist(strsplit(names, "[.]"))[-grep("R",unlist(strsplit(names, "[.]")))]
  
  if (length(name %in% c("R", "txt") >0)) {
    space <- which(name == "R")
    noloc <- which(name == "txt")
    name <- name[-c(space,noloc)]
  }
  total.time <- numeric()
  for (i in 1:length(names)){
    total.time[i] <- sum(values[which(fn == names[i])])
  }
  colourdata <- colouring()$colourdata
  total.sampling.time <- sum(values)
  names <- names[rev(order(total.time))]
  name <- name[rev(order(total.time))]
  total.time <- total.time[rev(order(total.time))]
  other <- list(total.time=total.time,total.sampling.time=total.sampling.time)
  test <-list(other=other)
  for (l in 1:length(names)){
     data <- cbind(newdetective(names[l],values,ln,fn,names,s,type="self", datawant=TRUE, styleswant=FALSE)[,c(1,10,11)], 
                   colourdata[match(newdetective(names[l],values,ln,fn,names,s,type="self", datawant=TRUE, styleswant=FALSE)[,11],letters)])
     names(data)[4] <- "colours"
     test[[names[l]]] <- data
  }
  test
return(test)
}

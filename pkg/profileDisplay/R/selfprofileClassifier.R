selfprofileClassifier <- function(prof="Rprof.out",filename #get lines input 
                                  # prof <- "profiling.txt"
                                  #filename <- getLines(c("Example.R","Example1.R","Example2.R"),"/Users/apt_imac")
                                  ,colourdata,...) {  
  s <- summaryRprof(prof, lines="show")
  total.sampling.time <- s$sampling.time
  loc <- rownames(s$by.line)
  fn <- sub("#.*","",loc)
  interval <- s$sample.interval
  names <- levels(factor(fn))
  k <- numeric()
  for(i in 1:length(names(filename))){
 j <-  grep(names(filename)[i],loc) 
 k <- c(k,j)
  }

  h <- s$by.line[k,]
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
  oldorder <- order(total.time, decreasing=TRUE)
 # names <- names[oldorder]
#  name <- name[oldorder]
#  total.time <- total.time[oldorder]
  other <- list(total.time=total.time,total.sampling.time=total.sampling.time)
  test <-list(other=other)
  for (l in oldorder){
    fn1 <- fn[grep(names[l],fn)]
    ln1 <- ln[grep(names[l],fn)]
    value <- values[grep(names[l],fn)]
     data <- cbind(newdetective(filename[l],value,ln1,fn1,names,s,type="self", datawant=TRUE, styleswant=FALSE)[,c(1,10,11)], 
                   colourdata[match(newdetective(filename[l],value,ln1,fn1,names,s,type="self", datawant=TRUE, styleswant=FALSE)[,11],letters)])
     names(data)[4] <- "colours"
     test[[names[l]]] <- data
  }
  test
return(test)
}

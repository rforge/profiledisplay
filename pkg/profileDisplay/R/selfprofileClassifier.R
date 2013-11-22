selfprofileClassifier <- function(prof = "Rprof.out",
                                  src = getLines(c("Example.R","Example1.R","Example2.R"),"/Users/apt_imac") 

                                  ,colourdata,...) {  
  if (is.character(prof))
    prof <- summaryRprof(prof, lines="show")
  total.sampling.time <- prof$sampling.time
  loc <- rownames(prof$by.line)
  fn <- sub("#.*","",loc)
  interval <- prof$sample.interval
  names <- levels(factor(fn))
  k <- numeric()
  for(i in 1:length(names(src))){
 j <-  grep(names(src)[i],loc) 
 k <- c(k,j)
  }

  h <- prof$by.line[k,]
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
  test <-list()
  for (l in oldorder){
    fn1 <- fn[grep(names[l],fn)]
    ln1 <- ln[grep(names[l],fn)]
    value <- values[grep(names[l],fn)]
     data <- cbind(newdetective(src[l],value,ln1,fn1,names,prof,type="self", datawant=TRUE, styleswant=FALSE)[,c(1,10,11)])
    data <- unique(data) 
    test[[names[l]]] <- data
  }
return(test)
}

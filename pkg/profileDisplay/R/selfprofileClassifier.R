selfprofileClassifier <- function(prof = "Rprof.out",
				  dir = ".",
                                  src = listRfiles(prof, dir),
                                  colourdata = colouring()$colourdata) {  
  if (is.character(prof))
    prof <- summaryRprof(prof, lines="show")
  total.sampling.time <- prof$sampling.time
  interval <- prof$sample.interval  
  loc <- rownames(prof$by.line)
  fn <- sub("#.*","",loc)
  names <- basename(src)
  keep <- fn %in% names
  h <- prof$by.line[keep,]
  
  values <- h$self.time
  loc <- rownames(h)
  fn <- sub("#.*","",loc)
  ln <- sub(".*#","",loc)
  total.time <- numeric(length(names))
  for (i in 1:length(names)){
    total.time[i] <- sum(values[which(fn == names[i])])
  }
  oldorder <- order(total.time, decreasing=TRUE)
  test <-list()
  for (l in oldorder){
    fn1 <- fn[grep(names[l],fn)]
    ln1 <- ln[grep(names[l],fn)]
    value <- values[grep(names[l],fn)]
    data <- cbind(newdetective(getLines(src[l]),value,ln1,fn1,names,prof,type="self", datawant=TRUE, styleswant=FALSE)[,c(1,10,11)])
    data <- unique(data) 
    test[[names[l]]] <- data
  }
  return(test)
}

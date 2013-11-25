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
  files <- list()
  titles <- character()
  info <- character()
  for (l in oldorder){
    fn1 <- fn[grep(names[l],fn)]
    ln1 <- ln[grep(names[l],fn)]
    value <- values[grep(names[l],fn)]
    lines <- getLines(src[l])
    data <- cbind(newdetective(lines,value,ln1,fn1,names,prof,type="self", datawant=TRUE, styleswant=FALSE)[,c(1,10,11)])
    data <- unique(data) 
    data$lines <- lines[[1]][data$line1]
    files[[names[l]]] <- data
    titles[names[l]] <- paste0("Self time profile data for ", names[l])
    info[names[l]] <- sprintf("Sampling interval:  %.2f  This file represents %.1f%% of total %.2fs execution time.", 
			       interval, 100*total.time[l]/total.sampling.time, total.sampling.time)
  }
  result <- structure(list(files = files, titles = titles, info = info, summaryRprof = prof),
                      class = "lineClassifier")
  return(result)
}

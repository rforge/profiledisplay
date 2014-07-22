expandGroups <- function(gp, lines) {
  p <- parse(text = lines)
  d <- getParseData(p)
  brace <- d$token == "'{'"
  block <- d$id %in% d$parent[brace]
  noncomment <- d$token != "COMMENT"
  statement <- (d$parent %in% unique(c(0,            # top level
                                      d$id[block]))) & # or block
	       !(d$token %in% c("'{'", "'}'"))      # but not the braces themselves
	       
  statements <- d[statement, ]   # only statements
  
  linenums <- seq_along(gp)
  start <- linenums %in% statements$line1
  continuation <- rep(FALSE, length(gp))
  for (s in seq_len(nrow(statements))) {
    first <- statements$line1[s]
    last <- statements$line2[s]
    nested <- (statements$line1 > first) & (statements$line2 <= last)
    last <- min(last, statements$line1[nested] - 1)
    if (last <= first) next
    nestedbrace <- brace & (d$line1 > first) & (d$line1 < last)
    last <- min(last, d$line1[nestedbrace])
    continuation <- continuation | ((first < linenums) 
                                  & (linenums <= last))
  }
  while (any(continuation)) {
    firstcont <- which(continuation)
    firstcont <- firstcont[!(firstcont - 1) %in% firstcont]
    gp[firstcont] <- gp[firstcont - 1]
    continuation[firstcont] <- FALSE
  }
  gp
}

selfprofileClassifier <- function(prof = "Rprof.out", dir = ".",
                                  src = listRfiles(prof, dir),
                                  colourdata = colouring()$colourdata
                                  ) {  
  if (is.character(prof))
    prof <- summaryRprof(prof, lines="show")
  force(src)
  profileClassifier("Self", prof, dir, src, colourdata)
}

totalprofileClassifier <- function(prof = "Rprof.out",
				  dir = ".",
                                  src = listRfiles(prof, dir),
                                  colourdata = colouring()$colourdata) {  
  if (is.character(prof))
    prof <- summaryRprof(prof, lines="show")
  force(src)
  profileClassifier("Total", prof, dir, src, colourdata)
}

profileClassifier <- function(profileType=c("Self","Total"), 
                              prof = "Rprof.out", 
			      dir = ".", 
			      src = listRfiles(prof, dir), 
            colourdata = colouring()$colourdata) {
  if (is.character(prof))
    prof <- summaryRprof(prof, lines="show")
    
  profileType <- match.arg(profileType)
			      
  total.sampling.time <- prof$sampling.time
  interval <- prof$sample.interval  
  times <- prof$by.line
  loc <- rownames(times)
  if (is.null(loc))
    stop("line profiling data not present")
  fn <- sub("#.*","",loc)
  names <- basename(src)
  keep <- fn %in% names
  times <- times[keep,]
  
  values <- switch(profileType, 
    Self = times$self.time,
    Total = times$total.time)
  loc <- loc[keep]
  fn <- sub("#.*","",loc)
  ln <- as.numeric(sub(".*#","",loc))
  
  total.time <- numeric(length(names))
  if (profileType=="self"){
    for (i in seq_along(names)){
      total.time[i] <- sum(times$self.pct[fn == names[i]])
    }
  }
  else{
    for (i in seq_along(names)){
      total.time[i] <- sum(times$total.pct[fn == names[i]])
    }
  }
  oldorder <- order(total.time, decreasing=TRUE)
  files <- list()
  titles <- character()
  info <- character()
  for (l in oldorder){
    lines <- getLines(src[l])[[1]]
    n <- length(lines)
    keep <- fn == names[l]
    ln1 <- ln[keep]
    if (any(ln1 > n)) {
	warning("profile line numbers exceed file length for ", names[l])
	keep <- keep && ln[keep] <= n
	ln1 <- ln[keep]
    }
    fn1 <- fn[keep]
    value <- values[keep]
    fullvalue <- numeric(n)
    fullvalue[ln1] <- value
    
    #choose breaks
   
    gp <-(11-cut(fullvalue, breaks=c(0,seq(1,100,10)), labels = FALSE,include.lowest=TRUE))
    
    # The group setting will be wrong for the zeros, because they'll include
    # multiline statements.  So parse the code and expand the grouping
    
    gp <- expandGroups(gp, lines)
    
    data <- data.frame(line1 = seq_along(lines), times = fullvalue, styles = gp, lines = lines)
    files[[names[l]]] <- data
    titles[names[l]] <- paste0(profileType, " time profile data for ", names[l])
    info[names[l]] <- sprintf("Sampling interval:  %.2f  This file represents %.1f%% of total %.2fs execution time.", 
			       interval, 100*total.time[l]/total.sampling.time, total.sampling.time)
  }
  result <- structure(list(files = files, titles = titles, info = info, summaryRprof = prof),
                      class = "lineClassifier", profileType = profileType  )
  return(result)
}

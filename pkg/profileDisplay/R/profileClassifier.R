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


profileClassifier <- function(prof = "Rprof.out",
                              profileType = c("Self","Total"), 
                              dir = ".", 
                              src = listRfiles(prof, dir), 
                              colourdata = colouring()$colourdata, 
                              breaks = 10 ) {
  if (is.character(prof))
    prof <- summaryRprof(prof, lines="show")
  
  profileType <- match.arg(profileType)
  
  total.sampling.time <- prof$sampling.time
  interval <- prof$sample.interval
  breakPoints<-c(0,seq(0.0001,100.0001, length.out = breaks+1))
  
  times <- prof$by.line
  loc <- rownames(times)
  if (is.null(loc))
    stop("line profiling data not present")
  fn <- sub("#.*","",loc)
  names <- basename(src)
  keep <- fn %in% names
  times <- times[keep,]
  
  values <- switch(profileType, 
                   Self = times$self.pct,
                   Total = times$total.pct)
  loc <- loc[keep]
  fn <- sub("#.*","",loc)
  ln <- as.numeric(sub(".*#","",loc))
  
  total.time <- numeric(length(names))
  if (profileType=="Self"){
    for (i in seq_along(names)){
      total.time[i] <- sum(times$self.pct[fn == names[i]])
    }
  }
  if (profileType=="Total"){
    for (i in seq_along(names)){
      total.time[i] <- max(times$total.pct[fn == names[i]])
    }
  }
  gpStyles <-(breaks+2-cut(total.time, breakPoints, labels = FALSE, include.lowest=TRUE, right=FALSE))
  
  oldorder <- order(total.time, decreasing=TRUE)
  files <- list()
  titles <- character()
  info <- character()
  fileStyles <- character()
  for (l in oldorder){
    fileStyles <- c(fileStyles, gpStyles[l])
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
    

    gp <-(breaks+2-cut(fullvalue, breakPoints, labels = FALSE, include.lowest=TRUE, right=FALSE))
  
    
    # The group setting will be wrong for the zeros, because they'll include
    # multiline statements.  So parse the code and expand the grouping
    
    gp <- expandGroups(gp, lines)
    data <- data.frame(line1 = seq_along(lines), times = fullvalue, styles = gp, lines = lines)
    files[[names[l]]] <- data
    titles[names[l]] <- paste0(profileType, " time profile data for ", names[l])
    info[names[l]] <- sprintf("Sampling interval:  %.2f  This file represents %.2f%% of total %.2fs execution time.", 
                              interval, total.time[l], total.sampling.time)
  }
  result <- structure(list(files = files, titles = titles, info = info, summaryRprof = prof),
                      class = "lineClassifier", profileType = profileType, ngp = breaks+1, fileStyles = fileStyles,
                      breakPoints = breakPoints)
  return(result)
}

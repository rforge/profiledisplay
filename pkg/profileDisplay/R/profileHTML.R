profileHTML <-
function(Rprof="Rprof.out", Rcode=NULL, detective = newdetective, int=12, colourdata=
           rgb(colorRamp(c("#FFFFD4" ,"#FED98E"), space="Lab")(0:11/11), maxColorValue=255), profileType=c("self","total", "memory"), openbrowser=TRUE, savefile=tempdir()) {
  if (int != length(colourdata)){int <- min(int, length(colourdata))}
  if (int <2){return(warning("At least 3 intervals must be produced."))}
  profileType <- match.arg(profileType)
  oldwd <- getwd()
    
  if (savefile != tempdir()){
    savefile <- normalizePath(savefile, winslash="/", mustWork=FALSE)
  }
  newpath <- savefile
  suppressWarnings(dir.create(newpath))
  
  if(is.null(Rcode)){
    if (grepl("www.", Rprof) | grepl("http://", Rprof)){
      download.file(url=Rprof,destfile=file.path(newpath,basename(Rprof))
      )
      Rprof <- file.path(newpath,basename(Rprof))
    }
  } else{
    Rprof <- NULL
    if (grepl("www.", Rcode)| grepl("http://", Rcode)){
      download.file(url=Rcode,destfile=file.path(newpath,basename(Rcode)))
      Rcode <- file.path(newpath,basename(Rcode))
    }
  }

  if (!is.null(Rprof)){
    Rprof <- normalizePath(Rprof, winslash="/", mustWork=FALSE)
    fname <- unlist(strsplit(basename(Rprof), "[.]"))[1]
    newpath <- file.path(newpath,paste("profileHTML",fname))
    if (file.exists(newpath)){
      unlink(file.path(newpath,list.files(newpath)), recursive=TRUE)
    }
    suppressWarnings(dir.create(newpath))
    file <- basename(Rprof)
    if(grepl("profiling: sample.interval=", readLines(Rprof)[1])){
      if(grepl("line profiling: sample.interval=", readLines(Rprof)[1])){
        newprof <- file.path(newpath,"Rprof.out")
        file.copy(Rprof, newprof)
      }else{
        return(warning("line profiling data is needed to continue."))
      }
  }
  }
  
  if (!is.null(Rcode)) {
    Rcode <- normalizePath(Rcode, winslash="/", mustWork=FALSE)
    fname <- unlist(strsplit(basename(Rcode), "[.]"))[1]
    newpath <- file.path(newpath,paste("profileHTML",fname))
    if (file.exists(newpath)){
      unlink(file.path(newpath,list.files(newpath)), recursive=TRUE)
    }
    suppressWarnings(dir.create(newpath))
    file <- basename(Rcode)
    Rprof(file.path(newpath, "Rprof.out"), line.profiling=TRUE, memory.profiling=TRUE)
    source(Rcode, echo=TRUE)
    Rprof(NULL)
  }
  
  if (profileType == "memory"){
    s <- summaryRprof(file.path(newpath, "Rprof.out"), lines="show", memory="both")          
  }else{
    s <- summaryRprof(file.path(newpath, "Rprof.out"), lines="show")
  }
  filevals <- readLines(file.path(newpath, "Rprof.out"))[grepl("#File",readLines(file.path(newpath, "Rprof.out")))]
  filevals <-   sub("^#File [0-9]: ", "", filevals)
  filevals <- filevals[file.exists(filevals)]
  filevalnames <- basename(filevals)
  if(length(filevalnames) != 0){
    for (i in 1:length(filevals)){
      suppressWarnings( writeLines(readLines(filevals[i]), con=file.path(newpath,filevalnames[i])))
    }
  }
  setwd(newpath)
  on.exit(setwd(oldwd))
  loc <- rownames(s$by.line)
  if (profileType == "self") {
    values <- s$by.line$self.time
    total.sampling.time <- s$sampling.time
  }
  if (profileType == "total") {
    values <- s$by.line$total.time
    total.sampling.time <- sum(values)
  }
  if (profileType == "memory") {
    values <- as.numeric(s$by.line$mem.total)
    total.sampling.time <- sum(values)
  }
  if (identical(colourdata,rgb(colorRamp(c("#FFFFD4" ,"#FED98E"), space="Lab")(0:11/11), maxColorValue=255))) {
    if (int != 12){
      colourdata <- rgb(colorRamp(c(colourdata[1] ,colourdata[length(colourdata)]), space="Lab")(0:(int-1)/(int-1)), maxColorValue=255)
      colouring(colourdata, int=int)
      cssloc <- file.path(getwd(), "test.css")
    }else{
      cssloc <- system.file("/css/newdetective.css", package="profileDisplay")
    }
  } else {
    colouring(colourdata, int=int)
    cssloc <- file.path(getwd(), "test.css")
  }
  col <- colourdata[int]
  
    
  
  myrenderer <- renderer_html(document=TRUE, 
                              stylesheet=cssloc)
  
  
  fn <- sub("#.*","",loc)
  ln <- sub(".*#","",loc)

  interval <- s$sample.interval

  names <- levels(factor(fn))


  
  test <- c("<no location>", "Tools.R")
  for (i in 1:length(test)){
  if (length(grep(test[i], names)) >0) {
    noloc <- which(names == "<no location>")
    tool <- which(names == "Tools.R")
    names <- names[-c(noloc,tool)]
  }
  }
  
  if (suppressWarnings(any(which(names == "")))){
    space <- which(names == "")
    names <- names[-space]
  }

  name <- unlist(strsplit(names, "[.]"))
  
  if (length(name %in% c("R", "txt") >0)) {
    space <- which(name == "R")
    noloc <- which(name == "txt")
    name <- name[-c(space,noloc)]
  }
  if (length(loc) < 1 | length(names) <1 ) {
    if (is.null(Rprof)){
      htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(Rcode), sep=""), local=TRUE)
    }
    if (is.null(Rcode)){
      htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(Rprof), sep=""), local=TRUE)
    }
    
    browseURL(url="summary.html")
    return(paste("The runtime for all lines is insignificantly small."))
  }
    total.time <- numeric()
    for (i in 1:length(names)){
      total.time[i] <- sum(values[which(fn == names[i])])
    }
    names <- names[rev(order(total.time))]
    name <- name[rev(order(total.time))]
    total.time <- total.time[rev(order(total.time))]
 
  namecode <- character()
  if (length(names) == 1){
    if(profileType == "memory"){
      for (j in 1:length(name)){
        namecode[j] <- paste("<b><a href=\"", name[j], ".html\" target=\"list\">",names[j],"</a></b> (",total.sampling.time," Mb)
  <br><i>Heavy Usage Lines</i><br>", sep="")
    }
      }else{
      for (j in 1:length(name)){
        namecode[j] <- paste("<b><a href=\"", name[j], ".html\" target=\"list\">",names[j],"</a></b> (",total.sampling.time," seconds)
  <br><i>Heavy Usage Lines</i><br>", sep="")
    }
    }
  } else{
    if(profileType == "memory"){
      for (j in 1:length(name)){
        namecode[j] <- paste("<b><a href=\"", name[j], ".html\" target=\"list\">",names[j],"</a></b> (",total.time[j]," Mb)
  <br><i>Heavy Usage Lines</i><br>", sep="")
      }
    }else{
      for (j in 1:length(name)){
        namecode[j] <- paste("<b><a href=\"", name[j], ".html\" target=\"list\">",names[j],"</a></b> (",total.time[j]," seconds)
  <br><i>Heavy Usage Lines</i><br>", sep="")
      }
    }
  }
namecode[length(namecode)+1] <- "<b><a href=\"summary.html\" target=\"list\">Summary Rprof Data</i></b>"
  
  if (!is.null(Rcode)){
    if (profileType == "memory"){
      htmlize(system.file("text/summarymemory.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(Rcode), sep=""), local=TRUE)
    }
    if (profileType == "self"){
      htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(Rcode), sep=""), local=TRUE)
    }
    if (profileType == "total"){
      htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(Rcode), sep=""), local=TRUE)
    }
  } else {
    if (profileType == "memory"){
      htmlize(system.file("text/summarymemory.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(Rprof), sep=""), local=TRUE)
    }
    if (profileType == "self"){
      htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(Rprof), sep=""), local=TRUE)
    }
    if (profileType == "total"){
      htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(Rprof), sep=""), local=TRUE)
    }
  }
  
  for (i in 1:length(name)) {
    colourer <- character()
   # timeintervals
    
    fn1 <- fn[grep(names[i],fn)]
    ln1 <- ln[grep(names[i],fn)]
    value <- values[grep(names[i],fn)]
    datavals <- newdetective(names[i],value=value, ln=ln1, fn=fn1, names=names, rprofdata=s, type=profileType, datawant=TRUE, styleswant=FALSE, int=int)[,c(1,10,11)]
    datavals <- unique(datavals)
    timeintervals <- seq(0, range(value)[2], length=(int-1))
    
    colourer<- highlight(output="profile.txt", parse.output=parse(names[i]), renderer=myrenderer, styles=detective(names[i], value=value, ln=ln1, fn=fn1, names=names, int=int, rprofdata=s, type=profileType), show_line_numbers=TRUE)
    measure <- linenumbers(colourer, int=int)
    measure[measure ==0] <- "none"
    colourer <- codecondenser(readLines("profile.txt"), int=int)
    par <- names[i]
    if (length(names) > 1){
      sampling.time <- total.time[i]
      if(profileType =="memory"){
        formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                            "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                            "</h1><p>For a sampling interval of ", interval," seconds, the total memory used was ", sampling.time," Mb.  Overall, the total memory used was ", total.sampling.time, " Mb.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
      }
      if(profileType =="self"){
        formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                            "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                            "</h1><p>For a sampling interval of ", interval," seconds, the total elapsed time was ", sampling.time," seconds.  Overall, the total runtime was ", total.sampling.time, " seconds, using self time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
      }
      if(profileType =="total"){
        formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                            "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                            "</h1><p>For a sampling interval of ", interval," seconds, the total elapsed time was ", sampling.time," seconds.  Overall, the total runtime was ", total.sampling.time, " seconds, using total time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
      }
       }else{
         if(profileType =="memory"){
           formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                               "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                               "</h1><p>For a sampling interval of ", interval," seconds, the total memory used was ", total.sampling.time," Mb.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
         }
         if(profileType =="self"){
           formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                               "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                               "</h1><p>For a sampling interval of ", interval," seconds, the total elapsed time was ", total.sampling.time," seconds, using self time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
         }
         if(profileType =="total"){
           formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                               "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                               "</h1><p>For a sampling interval of ", interval," seconds, the total elapsed time was ", total.sampling.time," seconds, using total time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
         }
    }
    writeLines(formatcode,"format.txt")
    form <- readLines("format.txt")
    maxi <- character()
    code <- colourer[grep("<a name=", colourer)]
    num <- sub("</span>.*","",code)
    num <- sub(".*<span class=\"line\">","",num)
    num <- sub("  ", "", num)
    num1 <- as.numeric(num)
    numb <- measure[measure != "none"]
        
    if (length(numb) >3) {
      if (int>5){
        alpha1 <- letters[(int-5):int]
        alpha1 <- paste("<span class=\"", alpha1, "\">", sep="")
        classmax <- alpha1[measure[(int-5):int] != "none"]
        classmax <- c(classmax[length(classmax)-1],max(classmax))
      } else{
        alpha1 <- letters[int]
        alpha1 <- paste("<span class=\"", alpha1, "\">", sep="")
        classmax <- alpha1[measure[int] != "none"]
        classmax <- c(classmax[length(classmax)-1],max(classmax))
      }
    } else {
      alpha1 <- paste("<span class=\"", letters[1:int], "\">", sep="")
      classmax <- alpha1[which(measure != "none")]
      if (length(grep("<span class=\"a\">", classmax)) > 0) {
        classmax <- classmax[-1]
      }
    }
    
    if (length(classmax) == 0){
      maxi <- paste("No lines")
    } else{
      for (b in 1:length(classmax)){
        if(all(diff(grep(classmax[b], code))==1)){
          first <- grep(classmax[b], code)[1]
          last<- grep(classmax[b], code)[length(grep(classmax[b], code))]
          if (identical(first,last)){
            if (profileType == "memory"){
              maxi[first] <- paste("<a href=\"", name[i], ".html#",num[first], "\" target=\"list\">", "Lines ", num1[first] ,"</a> (",datavals$time[which(datavals$line1 == num1[first])]," Mb)<br>", sep="")
            } else{
              maxi[first] <- paste("<a href=\"", name[i], ".html#",num[first], "\" target=\"list\">", "Lines ", num1[first] ,"</a> (",datavals$time[which(datavals$line1 == num1[first])]," seconds)<br>", sep="")
            }
            
          }else{
            if (profileType == "memory"){
              maxi[first] <- paste("<a href=\"", name[i], ".html#",num[first], "\" target=\"list\">", "Lines ", num1[first] ," to ", num1[last] ,"</a> (",datavals$time[which(datavals$line1 == num1[first])]," Mb)<br>", sep="")
            }
            else{
              maxi[first] <- paste("<a href=\"", name[i], ".html#",num[first], "\" target=\"list\">", "Lines ", num1[first] ," to ", num1[last] ,"</a> (",datavals$time[which(datavals$line1 == num1[first])]," seconds)<br>", sep="")
            } 
          }
           }else{
             if (profileType == "memory"){
               for (k in grep(classmax[b], code)){
                 maxi[k] <- paste("<a href=\"", name[i], ".html#",num[k], "\" target=\"list\">", "Line ", k ,"</a> (",datavals$time[which(datavals$line1 == num1[k])]," Mb)<br>", sep="")
               }
             } else{
               for (k in grep(classmax[b], code)){
                 maxi[k] <- paste("<a href=\"", name[i], ".html#",num[k], "\" target=\"list\">", "Line ", k ,"</a> (",datavals$time[which(datavals$line1 == num1[k])]," seconds)<br>", sep="")
               }
             }
          
      }
      }
      maxi <- maxi[!is.na(maxi)]
      if (length(grep("(NA seconds)", maxi)) > 0){
        maxi <- maxi[-grep("(NA seconds)", maxi)]
      }
      if (length(grep("(0 seconds)", maxi)) > 0){
        maxi <- maxi[-grep("(0 seconds)", maxi)]
      }
      maxi <- paste(maxi, collapse="")
    }

    namecode[i] <- paste(namecode[i],maxi, "<br>",sep="")
    colourer[which(colourer == "</style>")] <- form[1]#paste(colourer[c(which(colourer == "</style>"),which(colourer == "</head>"),which(colourer == "<body>"))],collapse="") 
    colourer[which(colourer == "</head>")] <- form[2]
    colourer[which(colourer == "<body>")] <- form[3]
    colourer[which(colourer == "<pre>")] <- form[4]
    

    alpha <- letters[1:int]
    table <- character()

    if (length(names) > 1){
      moretimevals <- seq(range(values)[1], range(values)[2], length=(int-1))
      ftimeintervals <- format(moretimevals, digits=2)
      timevec <- character()
      if (all(values == 0)) {
        timevec[values == moretimevals[1]] <- alpha[1]
      }else{
        for(l in 2:int){
          timevec[values >=moretimevals[l-1] & values <moretimevals[l]] <- alpha[l]
        }
        timevec[values == moretimevals[1]] <- alpha[1]
        timevec[values >= moretimevals[length(moretimevals)]] <- alpha[int]
      }
      measurer <- numeric()
      for (q in 1:length(alpha)){
        measurer[q] <- sum(timevec==alpha[q])
      }
      tot<- numeric()
    for (w in 1:length(names)){
      tot[w] <- length(readLines(names[w])) 
    }
measurer[1] <- sum(tot) - sum(measurer)
      
      measurer[measurer ==0] <- "none"
      for(l in 2:length(moretimevals)){
        table[l] <- paste("<tr><TD class=\"", alpha[l], "\"> </TD><td>", ftimeintervals[l-1], " to ",ftimeintervals[l],"</td><td>", measurer[l], "</td></tr>", sep="")
      }
      table[1] <- paste("<tr><TD class=\"", alpha[1], "\"> </TD><td>", ftimeintervals[1], "</td><td>", measurer[1], "</td></tr>", sep="")
      table[int] <- paste("<tr><TD class=\"", alpha[int], "\"> </TD><td>", ftimeintervals[int-1],"</td><td>", measurer[int], "</td></tr>", sep="")
      if(length(grep("none", table)) > 0) {
        table <- table[-grep("none",table)]
      }
      table <- paste(table, collapse="")
    } else{
      ftimeintervals <- format(timeintervals, digits=2)
      for(l in 2:length(timeintervals)){
        table[l] <- paste("<tr><TD class=\"", alpha[l], "\"> </TD><td>", ftimeintervals[l-1], " to ",ftimeintervals[l],"</td><td>", measure[l], "</td></tr>", sep="")
      }
      table[1] <- paste("<tr><TD class=\"", alpha[1], "\"> </TD><td>", ftimeintervals[1], "</td><td>", measure[1], "</td></tr>", sep="")
      table[int] <- paste("<tr><TD class=\"", alpha[int], "\"> </TD><td>", ftimeintervals[int-1],"</td><td>", measure[int], "</td></tr>", sep="")
      
      if(length(grep("none", table)) > 0) {
        table <- table[-grep("none",table)]
      }

      table <- paste(table, collapse="")
    }
  if(profileType == "memory"){
    formatcode3 <- paste("</pre>\n<a name=\"Legend\"></a><h1><i>Legend</i></h1></div><div><table border=\"2\" style=\"background-color:white;\">
      
      <tr>
      <th>Text Colour</th>
      <th>Approximate Amount of Memory Use Per Line (seconds)</th>
      <th># of Lines</th>
      </tr>", table, "</table>\n
</div>\n</body>\n</html>\n", sep="")
  } else {
    formatcode3 <- paste("</pre>\n<a name=\"Legend\"></a><h1><i>Legend</i></h1></div><div><table border=\"2\" style=\"background-color:white;\">
      
      <tr>
      <th>Text Colour</th>
      <th>Approximate Runtime Per Line (seconds)</th>
      <th># of Lines</th>
      </tr>", table, "</table>\n
</div>\n</body>\n</html>\n", sep="")
  }
    writeLines(formatcode3,"format1.txt")
    form1 <- readLines("format1.txt")
    colourer <- colourer[-((length(colourer)-2):length(colourer))]
    colourer <- c(colourer,form1)
  writeLines(colourer, paste(name[i],".html", sep=""))
  }
  if (sum(file == names) == 0) {
    writeLines(paste("<html><body bgcolor=\"#dddddd\"><center><h2>Table of Contents</h2></center>", paste(namecode, collapse=""), sep=""), con=file.path(paste("profiling_nav.html", sep="" )))
    writeLines(paste("<html><head><title>Syntax Highlighting Summary for R Profiling of " ,file, 
                     "</title></head><frameset cols=200,* border=1><frame src=\"", file.path(paste("profiling_nav.html", sep="" )), "\" name=\"nav\">
  <frame src=\"", file.path(paste(name[1], ".html", sep="" )), "\" name=\"list\" scrolling=yes>
  </frameset>
  </html>", sep=""), con=file.path("Rprof.html"))
  } else{
    writeLines(paste("<html><body bgcolor=\"#dddddd\"><center><h2>Table of Contents</h2></center>", paste(namecode, collapse=""), sep=""), con=file.path(paste(name[file == names], "_nav.html", sep="" )))
    writeLines(paste("<html><head><title>Syntax Highlighting Summary for R Profiling of " ,file, 
                     "</title></head><frameset cols=200,* border=1><frame src=\"", file.path(paste(name[file == names], "_nav.html", sep="" )), "\" name=\"nav\">
  <frame src=\"", file.path(paste(name[file == names], ".html", sep="" )), "\" name=\"list\" scrolling=yes>
  </frameset>
  </html>", sep=""), con=file.path("Rprof.html"))
    
  }
  
  files <- list.files()
keep <- c(files[grepl("html", files)], files[grepl("out", files)])
unlink(files[-match(keep, files)], recursive=TRUE)
  if (openbrowser){
    browseURL(url=file.path(getwd(), "Rprof.html"))
  }
  setwd(oldwd)
}

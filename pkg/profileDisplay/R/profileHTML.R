profileHTML <-
  function(Rprof="Rprof.out", Rcode=NULL, detective = newdetective, int=12, colourdata=
             rgb(colorRamp(c("#FFFFD4" ,"#FED98E"), space="Lab")(0:11/11), maxColorValue=255), 
           profileType=c("self","total", "memory"),savefile=tempdir()
           , visual="all", bar=FALSE) {
   ## if (int != length(colourdata)){int <- min(int, length(colourdata))}
 ##   if (int <2){return(warning("At least 3 intervals must be produced."))}
  ##  profileType <- match.arg(profileType)
    oldwd <- getwd()
    
    if (savefile != tempdir()){
      savefile <- normalizePath(savefile, winslash="/", mustWork=FALSE)
    }
    newpath <- savefile
    suppressWarnings(dir.create(newpath))
    
    if(is.null(Rcode)){
      fname <- unlist(strsplit(basename(Rprof), "[.]"))[1]
      path <- file.path(newpath,paste("profileHTML", unlist(strsplit(basename(Rprof), "[.]"))[1]))
      file <- basename(Rprof)
      s <- profilerOLD(Rprof=Rprof, Rcode=NULL, newpath, profileType=profileType)
    } else {
      fname <- unlist(strsplit(basename(Rcode), "[.]"))[1]
      path <- file.path(newpath,paste("profileHTML", unlist(strsplit(basename(Rcode), "[.]"))[1]))
      file <- basename(Rcode)
      s <-  profilerOLD(Rprof=NULL , Rcode=Rcode, newpath, profileType=profileType)
    }
    setwd(path)
    on.exit(setwd(oldwd))
    
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
    
    
    filevals <- readLines(file.path(path, "Rprof.out"))[grepl("#File",readLines(file.path(path, "Rprof.out")))]
    filevals <-   sub("^#File [0-9]: ", "", filevals)
    filevals <- filevals[file.exists(filevals)]
    filevalnames <- basename(filevals)
    if(length(filevalnames) != 0){
      for (i in 1:length(filevals)){
        suppressWarnings( writeLines(readLines(filevals[i]), con=file.path(path,filevalnames[i])))
      }
    }
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
    fn <- sub("#.*","",loc)
    ln <- sub(".*#","",loc)
    interval <- s$sample.interval
    names <- levels(factor(fn))
    
    ###Improve this section when finding names from Rprof Data
    
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
    ######
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
    ######
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
    
    
    if(visual == "summary") {
      browseURL(url="summary.html")
      return(paste(""))
    }
    
    for (i in 1:length(name)) {
      colourer <- character()
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
                              "</title><body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                              "</h1>\n<p>For a sampling interval of ", interval," seconds, the total memory used was ", sampling.time," Mb.  Overall, the total memory used was ", total.sampling.time, " Mb.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
        }
        if(profileType =="self"){
          formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                              "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                              "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", sampling.time," seconds.  Overall, the total runtime was ", total.sampling.time, " seconds, using self time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
        }
        if(profileType =="total"){
          formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                              "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                              "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", sampling.time," seconds.  Overall, the total runtime was ", total.sampling.time, " seconds, using total time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
        }
      }else{
        if(profileType =="memory"){
          formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                              "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                              "</h1>\n<p>For a sampling interval of ", interval," seconds, the total memory used was ", total.sampling.time," Mb.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
        }
        if(profileType =="self"){
          formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                              "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                              "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", total.sampling.time," seconds, using self time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
        }
        if(profileType =="total"){
          formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                              "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                              "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", total.sampling.time," seconds, using total time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
        }
      }
      writeLines(formatcode,"format.txt")
      form <- readLines("format.txt")
      
      maxi <- classdeterminer(colourer,measure,int, name[i],profileType, datavals)
      
      namecode[i] <- paste(namecode[i],maxi, "<br>",sep="")
      colourer[which(colourer == "</style>")] <- paste(form[1],form[2],sep="")#paste(colourer[c(which(colourer == "</style>"),which(colourer == "</head>"),which(colourer == "<body>"))],collapse="") 
      colourer[which(colourer == "</head>")] <- form[3]
      colourer[which(colourer == "<body>")] <- form[4]
      colourer[which(colourer == "<pre>")] <- form[5]
      if (visual == "table") {
        colourer <- colourer[-((grep("<p>", colourer)+1):(grep("</pre>", colourer)-1))]
      }
      if (visual == "code") {
        colourer <- colourer[-grep("<p>", colourer)]
      }
      if (visual == "info") {
        colourer <- colourer[-((grep("<p>", colourer)+2):(grep("</pre>", colourer)))]
      }
      if (visual == "all" | visual == "table") {
        formatcode3 <- tablemaker(names,values,int, timeintervals, profileType, measure)
      } else {
        formatcode3 <- "</pre>\n</div>\n</body>\n</html>\n"
      }
      
      writeLines(formatcode3,"format1.txt")
      form1 <- readLines("format1.txt")
      colourer <- colourer[-((length(colourer)-2):length(colourer))]
      colourer <- c(colourer,form1)
      if (visual == "all" | visual == "code") {
        if (bar == TRUE){
          colourer <- barview(int,colourer)
        }
      }

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

      if (visual == "table" | visual == "info") {
        if (sum(file == names) == 0) {
          browseURL(url=file.path(getwd(), paste(name[1], ".html", sep="" )))
        }else {
          browseURL(url=file.path(getwd(), paste(name[file == names], ".html", sep="" ))) 
        }
      } else {
        browseURL(url=file.path(getwd(), "Rprof.html"))
      }
      

    setwd(oldwd)
    }

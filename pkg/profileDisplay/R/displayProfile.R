displayprofile <- function(prof="Rprof.out", show=c("self","total","memory","executed"), lines, data,int=12,
                          savefile=tempdir()) {
  dir <- dirname(prof)
    if(dir=="."){
      dir <- getwd()
    }
  otherdata <- data$other
  data <- data[-1]
  s <- summaryRprof(prof, lines="show", memory="both")
  loc <- rownames(s$by.line)
  rowvals <- rownames(s$by.line)
  k <- numeric()
  for(i in 1:length(names(lines))){
    j <-  grep(names(lines)[i],loc) 
    k <- c(k,j)
  }
  
  h <- s$by.line[k,]
  values <- otherdata$values
  loc <- rownames(h)
  fn <- otherdata$fn
  ln <- otherdata$ln
  total.time <- otherdata$total.time
  oldorder <- order(total.time, decreasing=TRUE)
  li <- list()
  for (i in 1:length(oldorder)) {
   li[[i]] <- lines[[oldorder[i]]]
  }
names(li) <- names(data)
  lines <- li
  
  if (savefile != tempdir()){
    savefile <- normalizePath(savefile, winslash="/", mustWork=FALSE)
  }
  newpath <- savefile
  suppressWarnings(dir.create(newpath))
  fname <- unlist(strsplit(basename(prof), "[.]"))[1]
  path <- file.path(newpath,paste("profileHTML", unlist(strsplit(basename(prof), "[.]"))[1]))
  suppressWarnings(dir.create(path))
  file <- basename(prof)
  suppressWarnings( writeLines(readLines(file.path(dir,prof)), con=file.path(path,"Rprof.out")))
  oldwd <- getwd()
  setwd(path)
  on.exit(setwd(oldwd))

  #  getprofFiles(basename(prof), dir=dir)
  
col <- colouring()$col
    
  myrenderer <- renderer_html(document=TRUE, 
                    stylesheet=cssloc)
  htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(prof), sep=""), local=TRUE)
  
  names <- names(data)
  name <- unlist(strsplit(names, "[.]"))[-grep("R",unlist(strsplit(names, "[.]")))]
  total.sampling.time <- otherdata$total.sampling.time
  total.time <- total.time[order(total.time, decreasing=TRUE)]
  namecode <- character()
  if (length(names) == 1){
    if(show == "memory"){ ## if other profilers added, need to change, cannot be so rigid?
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
      if(show == "memory"){
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
  if (show == "executed"){
    htmlize(system.file(  , package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(prof), sep=""), local=TRUE)
  }
    if (show == "memory"){
      htmlize(system.file("text/summarymemory.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(prof), sep=""), local=TRUE)
    }
    if (show == "self"){
      htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(prof), sep=""), local=TRUE)
    }
    if (show == "total"){
      htmlize(system.file("text/summary.txt", package="profileDisplay"), "summary", HTMLdir=getwd(), title=paste("Summary Rprof Data for ",basename(prof), sep=""), local=TRUE)
    }
  for ( i in 1:length(names)){
    fn1 <- fn[grep(names[i],fn)]
    ln1 <- ln[grep(names[i],fn)]
    value <- values[grep(names[i],fn)]
    timeintervals <- seq(range(values)[1], range(values)[2], length=(int-1))
    colourer <- character()
    colourer<- highlight(output="profile.txt", parse.output=parse(text=lines[[i]]), renderer=myrenderer, styles=data[[i]]$styles, show_line_numbers=TRUE)
    measure <- linenumbers(colourer, int=int)
    colourer <- codecondenser(readLines("profile.txt"), int=int)
    par <- names[i]
    if (length(names) > 1){
      sampling.time <- total.time[i]
      if(show =="memory"){
        formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                            "</title><body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                            "</h1>\n<p>For a sampling interval of ", interval," seconds, the total memory used was ", sampling.time," Mb.  Overall, the total memory used was ", total.sampling.time, " Mb.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
      }
      if(show =="self"){
        formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                            "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                            "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", sampling.time," seconds.  Overall, the total runtime was ", total.sampling.time, " seconds, using self time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
      }
      if(show =="total"){
        formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                            "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                            "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", sampling.time," seconds.  Overall, the total runtime was ", total.sampling.time, " seconds, using total time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
      }
    }else{
      if(show =="memory"){
        formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                            "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                            "</h1>\n<p>For a sampling interval of ", interval," seconds, the total memory used was ", total.sampling.time," Mb.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
      }
      if(show =="self"){
        formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                            "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                            "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", total.sampling.time," seconds, using self time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
      }
      if(show =="total"){
        formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                            "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                            "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", total.sampling.time," seconds, using total time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
      }
    }
    writeLines(formatcode,"format.txt")
    form <- readLines("format.txt")
    maxi <- classdeterminer(colourer,measure,int, name[i],show, unique(data[[i]][,-4])) #change to generic
    namecode[i] <- paste(namecode[i],maxi, "<br>",sep="")
    colourer[which(colourer == "</style>")] <- paste(form[1],form[2],sep="")#paste(colourer[c(which(colourer == "</style>"),which(colourer == "</head>"),which(colourer == "<body>"))],collapse="") 
    colourer[which(colourer == "</head>")] <- form[3]
    colourer[which(colourer == "<body>")] <- form[4]
    colourer[which(colourer == "<pre>")] <- form[5]
    formatcode3 <- tablemaker(lines,values,int, timeintervals, show, measure)
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
  if (sum(file == names) == 0) {
    browseURL(url=file.path(getwd(), paste(name[1], ".html", sep="" )))
} else{
  browseURL(url=file.path(getwd(), "Rprof.html"))
}
  }
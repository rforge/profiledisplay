displayprofile <- function(prof="Rprof.out", show=c("self","total","memory","executed"), lines, data,
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
  total.time <- otherdata$total.time
  namecode <- character()
  if (length(names) == 1){
    if(show == "memory"){
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
    colourer <- character()
    colourer<- highlight(output="profile.txt", parse.output=parse(text=lines[[i]]), renderer=myrenderer, styles=data[[i]]$styles, show_line_numbers=TRUE)
    
  }
  }
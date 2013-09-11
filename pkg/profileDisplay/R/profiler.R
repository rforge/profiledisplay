profiler <- function(Rprof="Rprof.out", Rcode=NULL, newpath, profileType=c("self","total", "memory")) {
  
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
return(s)
}

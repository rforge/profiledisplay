getFiles <- function(Rprof="Rprof.out", dir = ".",pkg=NULL) {
  path <- paste("Files for profiling",basename(Rprof),sep=" ")
  dir.create(path)
  if(!is.null(pkg)){
    writeLines(getLines(Rcode,pkg=pkg)[[1]],con=file.path(path,Rcode))
    dir <- path
  }
  if(dir=="."){
    dir <- getwd()
  }
  oldwd <- getwd()
  setwd(path)
  on.exit(setwd(oldwd))
  file.copy(file.path(dir,Rprof),"Rprof.out")
  s <- summaryRprof("Rprof.out", lines="show", memory="both")
  d <- unique(sub("#.*","",rownames(s$by.line))[grep(".R",rownames(s$by.line))])
  for (i in 1:length(d)){
    writeLines(getLines(d[i],dir=dir)[[1]],con=basename(d[i]))
  }
  setwd(oldwd)
  return(s)
}

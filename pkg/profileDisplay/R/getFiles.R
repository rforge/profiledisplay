getFiles <- function(Rcode, dir = ".",pkg=NULL) {
  path <- paste("Files for profiling",basename(Rcode),sep=" ")
  dir.create(path)
  if(!is.null(pkg)){
    writeLines(getLines(Rcode,pkg=pkg)[[1]],con=file.path(path,Rcode))
    dir <- path
  }
  Rprof(filename=file.path(path,"Rprof.out"), line.profiling=TRUE, memory.profiling=TRUE)
  source(file.path(dir,Rcode), echo=TRUE)
  Rprof(NULL)
  oldwd <- getwd()
  setwd(path)
  on.exit(setwd(oldwd))
  s <- summaryRprof("Rprof.out", lines="show", memory="both")
  d <- unique(sub("#.*","",rownames(s$by.line))[grep(".R",rownames(s$by.line))])
  for (i in 1:length(d)){
    writeLines(getLines(d[i],dir=dir)[[1]],con=basename(d[i]))
  }
  setwd(oldwd)
  return(s)
}

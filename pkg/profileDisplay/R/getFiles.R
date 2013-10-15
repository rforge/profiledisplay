getFiles <- function(Rcode, dir = ".",pkg=NULL) {
  name <- Rcode[1]
  path <- paste("Files for profiling",basename(name),sep=" ")
  if (file.exists(path) == TRUE){
    stop("Folder already exists.")
  }
  suppressWarnings(dir.create(path))

  if(!is.null(pkg)){
    writeLines(getLines(Rprof,pkg=pkg)[[1]],con=file.path(path,Rprof))
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
}

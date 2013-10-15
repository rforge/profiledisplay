getprofFiles <- function(Rprof="Rprof.out", dir=".") {

  if(dir=="."){
  dir <- getwd()
}
  if (dir == getwd()){
    stop("Cannot write files to the same directory.")
  }
s <- summaryRprof(file.path(dir,Rprof), lines="show", memory="both")
d <- unique(sub("#.*","",rownames(s$by.line))[grep(".R",rownames(s$by.line))])
for (i in 1:length(d)){
  writeLines(getLines(d[i],dir=dir)[[1]],con=basename(d[i]))
}
}
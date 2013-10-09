createEx<-function(pkg=NULL){
  path <- normalizePath(getwd(), winslash="/", mustWork=FALSE)
  path<-file.path(path,paste(pkg,"-examples",sep=""))
  if (file.exists(path)){ unlink(file.path(path), recursive=TRUE) }
  suppressWarnings(dir.create(path))
  setwd(path)
  tools:::.createExdotR(pkg, system.file(packpromtage=pkg))
  newpath<-normalizePath(file.path(getwd(),list.files(path)), winslash="/", mustWork=FALSE)
  lines<-readLines(newpath)
  lines<-lines[!grepl("quit\\('no'\\)",lines)]
  writeLines(lines,con=newpath)
  return(newpath)
}
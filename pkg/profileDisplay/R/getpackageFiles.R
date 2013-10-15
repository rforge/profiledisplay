getpackageFiles <- function(Rcode, pkg) {
    for (i in 1:length(Rcode)){
      writeLines(getLines(Rcode,pkg=pkg)[[i]],con=file.path(path,Rcode[i]))
  }
}

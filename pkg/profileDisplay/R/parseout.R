parseout <- function(src){
  if (length(src)>1){
    parse.output <- parse(text=src)
    parsedata <- getParseData(parse.output)
  } else {
    parsedata <- list()
    for (i in 1:length(src)){
      parse.output <- parse(text=src[[i]])
      data <- getParseData(parse.output)
      parsedata[[names(src)[i]]] <- data
    }
  }
  return(parsedata)
}

codecondenser <- function(htmldata, int=12) {
  comm <- character()
  alpha <- letters[1:int]
  
  if (length(which(htmldata == "")) != 0){
    htmldata <- htmldata[-which(htmldata == "")]
  }
  
  for (j in 1:length(alpha)) {
    numbers <- grep(paste("<span class=\"", alpha[j], "\">", sep=""), htmldata)
    for (i in numbers){
      data <- unlist(strsplit(htmldata[i], "</span>"))
      data[1] <- paste(data[1],"</span>", sep="") #add space after <> ?
      data[length(data)] <- paste(data[length(data)],"</span>", sep="")
      data <- paste(data,collapse="")
      data <- unlist(strsplit(data, paste("<span class=\"", alpha[j], "\">", sep="")))
      data[1] <- paste(data[1],paste("<span class=\"", alpha[j], "\">", sep=""), sep="")
      data <- paste(data,collapse="")
      htmldata[i] <- data
    }
    miss <- grep("<span class=\"line\">", htmldata)
    gone <- missingseq(miss)
    find <- intersect(numbers,gone)
    if (length(find)>0){
      for (k in find){
        data <- unlist(strsplit(htmldata[k], "</span>"))
        data[length(data)] <- paste(data[length(data)],"</span>", sep="")
        data <- unlist(strsplit(data, paste("<span class=\"", alpha[j], "\">", sep="")))
        data <- unlist(strsplit(data, " "))
        data[which(data != "")[1]] <- paste(paste("<span class=\"", alpha[j], "\">", sep=""),
                                            data[which(data != "")[1]],sep="")
        data <- paste(data,collapse=" ")
        htmldata[k] <- data
      }
    }
  }
  for(k in 1:length(grep("<span class=\"line\">", htmldata))){
    num <- sub("</span>.*","",htmldata)
    num <- sub(".*<span class=\"line\">","",num)
    num <- sub("  ", "", num)
    htmldata[grep("<span class=\"line\">", htmldata)[k]] <- paste("<a name=\"", num[grep("<span class=\"line\">", htmldata)[k]],"\"></a>", htmldata[grep("<span class=\"line\">", htmldata)[k]], sep="")
  }
    return(htmldata)
    }
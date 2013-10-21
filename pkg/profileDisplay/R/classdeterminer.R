classdeterminer <- function(colourer,measure,int, name,profileType, datavals) {
  maxi <- character()
  code <- colourer[grep("<a name=", colourer)]
  num <- sub("</span>.*","",code)
  num <- sub(".*<span class=\"line\">","",num)
  num <- sub("  ", "", num)
  num1 <- as.numeric(num)
  numb <- measure[measure != 0]
  datavals <- unique(datavals)
  if (length(numb) >3) {
    if (int>5){
      alpha1 <- letters[(int-5):int]
      alpha1 <- paste("<span class=\"", alpha1, "\">", sep="")
      classmax <- alpha1[measure[(int-5):int] != 0]
      classmax <- c(classmax[length(classmax)-1],max(classmax))
    } else{
      alpha1 <- letters[int]
      alpha1 <- paste("<span class=\"", alpha1, "\">", sep="")
      classmax <- alpha1[measure[int] != 0]
      classmax <- c(classmax[length(classmax)-1],max(classmax))
    }
  } else {
    alpha1 <- paste("<span class=\"", letters[1:int], "\">", sep="")
    classmax <- alpha1[which(measure != 0)]
    if (length(grep("<span class=\"a\">", classmax)) > 0) {
      classmax <- classmax[-1]
    }
  }
  
  if (length(classmax) == 0){
    maxi <- paste("No lines")
  } else{
    for (b in 1:length(classmax)){
      if(all(diff(grep(classmax[b], code))==1)){
        first <- grep(classmax[b], code)[1]
        last<- grep(classmax[b], code)[length(grep(classmax[b], code))]
        if (identical(first,last)){
          if (profileType == "memory"){
            maxi[first] <- paste("<a href=\"", name, ".html#",num[first], "\" target=\"list\">", "Lines ", num1[first] ,"</a> (",datavals$time[which(datavals$line1 == num1[first])]," Mb)<br>", sep="")
          } else{
            maxi[first] <- paste("<a href=\"", name, ".html#",num[first], "\" target=\"list\">", "Lines ", num1[first] ,"</a> (",datavals$time[which(datavals$line1 == num1[first])]," seconds)<br>", sep="")
          }
          
        }else{
          if (profileType == "memory"){
            maxi[first] <- paste("<a href=\"", name, ".html#",num[first], "\" target=\"list\">", "Lines ", num1[first] ," to ", num1[last] ,"</a> (",datavals$time[which(datavals$line1 == num1[first])]," Mb)<br>", sep="")
          }
          else{
            maxi[first] <- paste("<a href=\"", name, ".html#",num[first], "\" target=\"list\">", "Lines ", num1[first] ," to ", num1[last] ,"</a> (",datavals$time[which(datavals$line1 == num1[first])]," seconds)<br>", sep="")
          } 
        }
      }else{
        if (profileType == "memory"){
          for (k in grep(classmax[b], code)){
            maxi[k] <- paste("<a href=\"", name, ".html#",num[k], "\" target=\"list\">", "Line ", k ,"</a> (",datavals$time[which(datavals$line1 == num1[k])]," Mb)<br>", sep="")
          }
        } else{
          for (k in grep(classmax[b], code)){
            maxi[k] <- paste("<a href=\"", name, ".html#",num[k], "\" target=\"list\">", "Line ", k ,"</a> (",datavals$time[which(datavals$line1 == num1[k])]," seconds)<br>", sep="")
          }
        }
        
      }
    }
    maxi <- maxi[!is.na(maxi)]
    if (length(grep("(NA seconds)", maxi)) > 0){
      maxi <- maxi[-grep("(NA seconds)", maxi)]
    }
    if (length(grep("(0 seconds)", maxi)) > 0){
      maxi <- maxi[-grep("(0 seconds)", maxi)]
    }
    maxi <- paste(maxi, collapse="")
  }
  return(maxi)
}
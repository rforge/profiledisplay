namecode <- function(show,names,otherdata){
  name <- unlist(strsplit(names, "[.]"))[-grep("R",unlist(strsplit(names, "[.]")))]
  total.sampling.time <- otherdata$total.sampling.time
  total.time <- total.time[order(total.time, decreasing=TRUE)]
  namecode <- character()
  if (length(names) == 1){
    if(show == "memory"){ ## if other profilers added, need to change, cannot be so rigid?
      for (j in 1:length(name)){
        namecode[j] <- paste("<b><a href=\"", name[j], ".html\" target=\"list\">",names[j],"</a></b> (",total.sampling.time," Mb)
                             <br><i>Heavy Usage Lines</i><br>", sep="")
      }
      }else{
        for (j in 1:length(name)){
          namecode[j] <- paste("<b><a href=\"", name[j], ".html\" target=\"list\">",names[j],"</a></b> (",total.sampling.time," seconds)
                               <br><i>Heavy Usage Lines</i><br>", sep="")
        }
    }
} else{
  if(show == "memory"){
    for (j in 1:length(name)){
      namecode[j] <- paste("<b><a href=\"", name[j], ".html\" target=\"list\">",names[j],"</a></b> (",total.time[j]," Mb)
                               <br><i>Heavy Usage Lines</i><br>", sep="")
    }
  }else{
    for (j in 1:length(name)){
      namecode[j] <- paste("<b><a href=\"", name[j], ".html\" target=\"list\">",names[j],"</a></b> (",total.time[j]," seconds)
                                 <br><i>Heavy Usage Lines</i><br>", sep="")
    }
  }
}
  return(namecode)
}
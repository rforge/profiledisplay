tablemaker <- function(names,values,int=12, timeintervals, profileType, measure) {
  alpha <- letters[1:int]
  table <- character()
  if (length(names) > 1){
    moretimevals <- seq(range(values)[1], range(values)[2], length=(int-1))
    ftimeintervals <- format(moretimevals, digits=2)
    timevec <- character()
    if (all(values == 0)) {
      timevec[values == moretimevals[1]] <- alpha[1]
    }else{
      for(l in 2:int){
        timevec[values >=moretimevals[l-1] & values <moretimevals[l]] <- alpha[l]
      }
      timevec[values == moretimevals[1]] <- alpha[1]
      timevec[values >= moretimevals[length(moretimevals)]] <- alpha[int]
    }
    measurer <- numeric()
    for (q in 1:length(alpha)){
      measurer[q] <- sum(timevec==alpha[q])
    }
    tot<- numeric()
    for (w in 1:length(names)){
      tot[w] <- length(readLines(names[w])) 
    }
    measurer[1] <- sum(tot) - sum(measurer)
    
    measurer[measurer ==0] <- "none"
    for(l in 2:length(moretimevals)){
      table[l] <- paste("<tr><TD class=\"", alpha[l], "\"> </TD><td>", ftimeintervals[l-1], " to ",ftimeintervals[l],"</td><td>", measurer[l], "</td></tr>", sep="")
    }
    table[1] <- paste("<tr><TD class=\"", alpha[1], "\"> </TD><td>", ftimeintervals[1], "</td><td>", measurer[1], "</td></tr>", sep="")
    table[int] <- paste("<tr><TD class=\"", alpha[int], "\"> </TD><td>", ftimeintervals[int-1],"</td><td>", measurer[int], "</td></tr>", sep="")
    if(length(grep("none", table)) > 0) {
      table <- table[-grep("none",table)]
    }
    table <- paste(table, collapse="")
  } else{
    ftimeintervals <- format(timeintervals, digits=2)
    for(l in 2:length(timeintervals)){
      table[l] <- paste("<tr><TD class=\"", alpha[l], "\"> </TD><td>", ftimeintervals[l-1], " to ",ftimeintervals[l],"</td><td>", measure[l], "</td></tr>", sep="")
    }
    table[1] <- paste("<tr><TD class=\"", alpha[1], "\"> </TD><td>", ftimeintervals[1], "</td><td>", measure[1], "</td></tr>", sep="")
    table[int] <- paste("<tr><TD class=\"", alpha[int], "\"> </TD><td>", ftimeintervals[int-1],"</td><td>", measure[int], "</td></tr>", sep="")
    
    if(length(grep("none", table)) > 0) {
      table <- table[-grep("none",table)]
    }
    
    table <- paste(table, collapse="")
  }
  if(profileType == "memory"){
    formatcode3 <- paste("</pre>\n<a name=\"Legend\"></a><h1><i>Legend</i></h1></div><div><table border=\"2\" style=\"background-color:white;\">
                             
                             <tr>
                             <th>Text Colour</th>
                             <th>Approximate Amount of Memory Use Per Line (seconds)</th>
                             <th># of Lines</th>
                             </tr>", table, "</table>\n
                             </div>\n</body>\n</html>\n", sep="")
  } else {
    formatcode3 <- paste("</pre>\n<a name=\"Legend\"></a><h1><i>Legend</i></h1></div><div><table border=\"2\" style=\"background-color:white;\">
                             
                             <tr>
                             <th>Text Colour</th>
                             <th>Approximate Runtime Per Line (seconds)</th>
                             <th># of Lines</th>
                             </tr>", table, "</table>\n
                             </div>\n</body>\n</html>\n", sep="")
  }
  return(formatcode3)
}
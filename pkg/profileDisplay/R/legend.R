legend <- function(name,data,classes,...){
  
  files <- data$files
  titles <- data$titles
  info <- data$info
    files <- files[[which(names(files)==name)]]
    styles <- files[,3]
    totallines <- table(styles)
    timeintervals <- seq(0,max(files$times), length=max(styles)-1)
    rowtab <- paste("<tr> <td class=\"",classes[1],"\"></td><td>  0</td><td>",totallines[1],"</td></tr>",sep="")
    check <- sort(unique(styles[-c(which(styles == 1),which(styles == max(styles)))]))
    for (i in check){
      rowtab1 <- paste("<tr> <td class=\"",classes[i],"\"></td><td>",timeintervals[i-1]," to ",timeintervals[i],"</td><td>",totallines[i],"</td></tr>",sep="")
      rowtab <- c(rowtab,rowtab1)
    }
    rowtabf <- paste("<tr> <td class=\"",classes[length(classes)],"\"></td><td>", timeintervals[length(timeintervals)] ,"</td><td>",totallines[length(totallines)],"</td></tr>",sep="")
rowtab <- c(rowtab,rowtabf)
    cat("</pre>\n<a name=\"Legend\"></a><h1><i>Legend</i></h1></div><div><table border=\"2\" style=\"background-color:white;\">",
        "<tr>
          <th>Text Colour</th>
          <th>Approximate Runtime Per Line (seconds)</th>
          <th># of Lines</th>                             </tr>",rowtab, "</table>\n
                             </div>\n</body>\n</html>\n")
}
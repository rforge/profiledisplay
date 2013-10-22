headerHTML <- function(show, names,otherdata,interval, i) {
  total.time <- otherdata$total.time
  oldorder <- order(total.time, decreasing=TRUE)
  total.time <- total.time[oldorder]
  total.sampling.time <- otherdata$total.sampling.time
  par <- names[i]
  sampling.time <- total.time[i]
  col <- otherdata$colourdata[length(otherdata$colourdata)]
  if (length(names) > 1){
    if(show =="memory"){
      formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                          "</title><body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                          "</h1>\n<p>For a sampling interval of ", interval," seconds, the total memory used was ", sampling.time," Mb.  Overall, the total memory used was ", total.sampling.time, " Mb.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
    }
    if(show =="self"){
      formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                          "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                          "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", sampling.time," seconds.  Overall, the total runtime was ", total.sampling.time, " seconds, using self time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
    }
    if(show =="total"){
      formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                          "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                          "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", sampling.time," seconds.  Overall, the total runtime was ", total.sampling.time, " seconds, using total time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
    }
  }else{
    if(show =="memory"){
      formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                          "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                          "</h1>\n<p>For a sampling interval of ", interval," seconds, the total memory used was ", total.sampling.time," Mb.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
    }
    if(show =="self"){
      formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                          "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                          "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", total.sampling.time," seconds, using self time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
    }
    if(show =="total"){
      formatcode <- paste("</style></head><body>\n<title>Syntax Highlighting Summary for R Profiling of " ,par, 
                          "</title>\n<body bgcolor=\"#dddddd\"><div id=\"header\"><h1>R Profiling Summary for ", par, 
                          "</h1>\n<p>For a sampling interval of ", interval," seconds, the total elapsed time was ", total.sampling.time," seconds, using total time.</p>\n<a name=\"Code\"></a><h1><i>Code</i></h1><pre style=\"background:",col,";border-style:double;border-width:5px;\">\n", sep="")
    }
  }
  writeLines(formatcode,"format.txt")
  form <- readLines("format.txt")
  return(form)
}
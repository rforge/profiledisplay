tocHTML <- function(prof,data,namecode){
  names <- names(data)
  name <- unlist(strsplit(names, "[.]"))[-grep("R",unlist(strsplit(names, "[.]")))]
  if (sum(prof == names) == 0) {
    writeLines(paste("<html><body bgcolor=\"#dddddd\"><center><h2>Table of Contents</h2></center>", paste(namecode, collapse=""), sep=""), con=file.path(paste("profiling_nav.html", sep="" )))
    writeLines(paste("<html><head><title>Syntax Highlighting Summary for R Profiling of " ,prof, 
                     "</title></head><frameset cols=200,* border=1><frame src=\"", file.path(paste("profiling_nav.html", sep="" )), "\" name=\"nav\">
                       <frame src=\"", file.path(paste(name[1], ".html", sep="" )), "\" name=\"list\" scrolling=yes>
                       </frameset>
                       </html>", sep=""), con=file.path("Rprof.html"))
  } else{
    writeLines(paste("<html><body bgcolor=\"#dddddd\"><center><h2>Table of Contents</h2></center>", paste(namecode, collapse=""), sep=""), con=file.path(paste(name[prof == names], "_nav.html", sep="" )))
    writeLines(paste("<html><head><title>Syntax Highlighting Summary for R Profiling of " ,prof, 
                     "</title></head><frameset cols=200,* border=1><frame src=\"", file.path(paste(name[prof == names], "_nav.html", sep="" )), "\" name=\"nav\">
                       <frame src=\"", file.path(paste(name[prof == names], ".html", sep="" )), "\" name=\"list\" scrolling=yes>
  </frameset>
  </html>", sep=""), con=file.path("Rprof.html"))
    
  }
}
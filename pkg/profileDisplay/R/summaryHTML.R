summaryHTML <- function(prof = "Rprof.out", 
                        show = c("self", "total", "memory"), 
			dir = tempdir()){
  
  show <- match.arg(show)
  
  memory <- ifelse(show == "memory", "both", "none")
  
  if (is.character(prof))
    prof <- summaryRprof(prof, lines = "show", memory = memory)
    
  rownames(prof$by.self)[rownames(prof$by.self)=="<no location>"] <- "\"no location\""
  rownames(prof$by.total)[rownames(prof$by.total)=="<no location>"] <- "\"no location\""
  rownames(prof$by.line)[rownames(prof$by.line)=="<no location>"] <- "\"no location\""
    
  outfile <- "summary.html"
  outcon <- file( file.path(dir, outfile), "w")
  StartList(outcon, title="Summary Rprof Data") 
  cat("<pre>\n", file = outcon)
  sink(outcon)
  on.exit({ sink(NULL); close(outcon) } )
  print(prof)
  sink(NULL)
  cat("</pre>\n", file = outcon)
  EndHTML(outcon)
  close(outcon)
  on.exit()
  outfile
}

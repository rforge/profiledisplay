summaryToHTML <- function(prof = "Rprof.out", 
                        show = c("self", "total", "memory"), 
			dir = tempdir(),
			outfile = "summaryRprof.html"){
  
  show <- match.arg(show)
  
  memory <- ifelse(show == "memory", "both", "none")
  
  if (is.character(prof))
    prof <- summaryRprof(prof, lines = "show", memory = memory)
    
  rownames(prof$by.self)[rownames(prof$by.self)=="<no location>"] <- "\"no location\""
  rownames(prof$by.total)[rownames(prof$by.total)=="<no location>"] <- "\"no location\""
  rownames(prof$by.line)[rownames(prof$by.line)=="<no location>"] <- "\"no location\""
    
  outfile <- file.path(dir, outfile)
  htmlcon <- file(outfile, "wt")
  on.exit(close(htmlcon))
  cat(file = htmlcon,
    '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">',
    '<html><head>', 
    '<title>summarRprof data</title>',
    '</head>',
    '<body>',
    '<pre>',
    capture.output(print(prof)),
    '</pre>',
    '</body></html>',
    '', sep="\n")
  outfile
}

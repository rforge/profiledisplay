# This function is like the one in tools
htmlify <- function (x) 
{
    x <- gsub("&", "&amp;", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("---", "&mdash;", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("--", "&ndash;", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("``", "&ldquo;", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("''", "&rdquo;", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("`([^']+)'", "&lsquo;\\1&rsquo;", x, perl = TRUE, useBytes = TRUE)
    x <- gsub("`", "'", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("\"\\{\"", "\"{\"", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE, useBytes = TRUE)
    x
}

toHTML.lineClassifier <- function(x, profileType, htmldir = tempdir(), ...) {
  stopifnot("lineClassifier" %in% class(x))
  files <- x$files
  titles <- x$titles
  info <- x$info
  filenames <- names(files)
  doSummary <- !is.null(x$summaryRprof)
  if (doSummary) {
    filenames <- c(filenames, "summaryRprof")
    info <- c(info, "")
    titles <- c(titles, "Raw summaryRprof data")
  }
  htmlfilenames <- paste0(filenames, ".html")
  
  allclasses <- c()    
  for (i in seq_along(filenames)) {
    name <- filenames[i]
    htmlcon <- file( file.path(htmldir, htmlfilenames[i]), "wt")
    on.exit(close(htmlcon))
    cat(file = htmlcon,
        '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">',
        '<html><head>', 
        paste0('<title>', titles[i], '</title>'),
        '<link rel="stylesheet" type="text/css" href="profileDisplay.css">',
        '</head>',
        '<body>',
        '<div id="header">',
        paste0('<h1 style="margin-bottom:0">', titles[i], '</h1>'),
        '</div>',
        '<div id="TOC" style="width:100px; float:left;">',
        '<br>',
        '<b>Files</b><br>',
        paste0('<a href="', htmlfilenames, '">', filenames, '</a><br>'),
        '</div>',
        '<div id="listing">',
        '<br>',
        info[i],
        if (!doSummary || i < length(filenames))
          paste0('<h2>Code</h2>'),
        '<pre>',
        '', sep="\n")
    
    if (doSummary && i == length(filenames)) 
      {lines <- htmlify( capture.output( print(x$summaryRprof) ) );table=""}
    else {
      lines <- files[[i]]$lines
      styles <- files[[i]]$styles
      if(profileType=="Executed") table<-maketable(styles)
      classes <- paste0("gp", styles)
      allclasses <- unique(c(allclasses, styles))
      linenum <- files[[i]]$line1
      linenum <- sprintf("%0*d  ", floor(log10(max(linenum))) + 1, linenum)
      regexp <- "(^\\s*)(\\S*.*\\S*)(\\s*$)"
      pre <- sub(regexp, "\\1", lines)
      mid <- sub(regexp, "\\2", lines)
      post <- sub(regexp, "\\3", lines)
      
      lines <- paste0(linenum, pre, '<span class="', classes, '">', htmlify(mid), '</span>', post)
    }
    cat(file = htmlcon,
        lines,
        '</pre>',
        table,
        '</div>',
        '</body></html>',
        '', sep="\n")
    close(htmlcon)
    on.exit()
  }
  writeCSS(paste0("gp", sort(allclasses)), file.path(htmldir, "profileDisplay.css"), ...)
  file.path(htmldir, htmlfilenames[1])
}

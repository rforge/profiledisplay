toHTML.lineClassifier <- function(x, htmldir = tempdir(), ...) {
    
    stopifnot("lineClassifier" %in% class(x))
    
    files <- x$files
    titles <- x$titles
    info <- x$info
    filenames <- names(files)
    htmlfilenames <- paste0(filenames, ".html")
    
    allclasses <- c()    
    for (i in seq_along(files)) {
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
	    paste0('<h2>Code</h2>'),
	    '<pre>',
	    '', sep="\n")
	    
	lines <- files[[i]]$lines
	classes <- files[[i]]$styles
	allclasses <- unique(c(allclasses, classes))
        linenum <- files[[i]]$line1
	linenum <- sprintf("%0*d  ", floor(log10(max(linenum))) + 1, linenum)
	regexp <- "(^\\s*)(\\S*.*\\S*)(\\s*$)"
	pre <- sub(regexp, "\\1", lines)
	mid <- sub(regexp, "\\2", lines)
	post <- sub(regexp, "\\3", lines)
	
	lines <- paste0(linenum, pre, '<span class="', classes, '">', mid, '</span>', post)
	cat(file = htmlcon,
	    lines,
	    '', sep="\n")
	    
	cat(file = htmlcon,
	    '</pre>',
	    '</div>',
	    '</body></html>',
	    '', sep="\n")
	close(htmlcon)
	on.exit()
    }
    writeCSS(sort(allclasses), file.path(htmldir, "profileDisplay.css"), ...)
    file.path(htmldir, htmlfilenames[1])
}

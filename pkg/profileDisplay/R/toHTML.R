toHTML.lineClassifier <- function(x, htmldir = tempdir(), ...) {
    
    stopifnot("lineClassifier" %in% class(x))
    
    files <- x$files
    titles <- x$titles
    info <- x$info
    
    allclasses <- c()
    
    for (i in seq_along(files)) {
	name <- names(files)[i]
	htmlcon <- file( file.path(htmldir, paste0(name, ".html")), "wt")
	on.exit(close(htmlcon))
	cat(file = htmlcon,
	    '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">',
	    '<html><head>', 
	    paste0('<title>', titles[i], '</title>'),
	    '<link rel="stylesheet" type="text/css" href="profileDisplay.css">',
	    '</head>',
	    '<body>',
	    paste0('<h1>', titles[i], '</h1>'),
	    info[i],
	    paste0('<h2>Code</h2>'),
	    '<pre>',
	    '', sep="\n")
	    
	lines <- files[[i]]$lines
	classes <- files[[i]]$styles
	allclasses <- unique(c(allclasses, classes))
	
	regexp <- "(^\\s*)(\\S*.*\\S*)(\\s*$)"
	pre <- sub(regexp, "\\1", lines)
	mid <- sub(regexp, "\\2", lines)
	post <- sub(regexp, "\\3", lines)
	
	lines <- paste0(pre, '<span class="', classes, '">', mid, '</span>', post)
	cat(file = htmlcon,
	    lines,
	    '', sep="\n")
	    
	cat(file = htmlcon,
	    '</pre>',
	    '</body></html>',
	    '', sep="\n")
	close(htmlcon)
	on.exit()
    }
    writeCSS(sort(allclasses), file.path(htmldir, "profileDisplay.css"))
    htmldir
}

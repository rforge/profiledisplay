writeCSS <- function(classes,
                      filename,
                      bg = c("#FF3030","white"), 
		      fg = c("black", "black"),
		      border.bg = "#FED98E") {
    n <- length(classes)
    bg <- rgb(colorRamp(bg, space = "Lab")(c(seq(0, 0.9, len=n-1),1)), maxColorValue=255)
    fg <- rgb(colorRamp(fg, space = "Lab")(c(seq(0, 0.9, len=n-1),1)), maxColorValue=255)
    
    con <- file(filename, "wt")
    on.exit(close(con))
    cat(file = con,
	paste0(".", classes, '{background-color:', bg, '; color:', fg, ';}'),
	paste0('#header{background-color:', border.bg, ';}'),
	paste0('#TOC{background-color:', border.bg, '; height:90%; float:left;}'),
	'#listing{margin-left:20px; float:left;}',
	'', sep = "\n")
    filename
}

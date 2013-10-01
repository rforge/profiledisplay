getLines <- function(filename, dir = ".", pkg = NULL) {
    if (!is.null(pkg)) {
        stopifnot(length(pkg) == 1L)
	istarfile <- grepl("[.]tar[.]gz$", pkg)
	if (!istarfile) {
	    message("downloading package source\n")
	    downloaded <- download.packages(pkg, tempdir(), type = "source")
	    if (!nrow(downloaded)) return(character())
	    tarfile <- downloaded[,2]
	} else
	    tarfile <- pkg
        tardir <- tempfile("untar")
	untar(tarfile, exdir = tardir)
	dir <- file.path(list.files(tardir, full.names = TRUE), "R")
    }
    fullname <- file.path(dir, filename)
    exists <- file.exists(fullname)
    if (any(!exists)) {
	msg <- paste0("file(s)\n", paste(" ", filename, collapse="\n"), "\ndo not exist in ", dir)
	if (all(!exists)) stop(msg)
	warning(msg)
    }
    result <- lapply(fullname[exists], readLines)
    names(result) <- filename
    result
}
	    
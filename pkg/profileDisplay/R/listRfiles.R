expandFilenames <- function(names, dirs = ".") {
  names <- gsub("\\", "/", names, fixed = TRUE)
  dirs <- gsub("\\", "/", dirs, fixed = TRUE)
  
  matched <- character()  
  for (dir in dirs) {
    fullnames <- ifelse(grepl("/", names), names, file.path(dir, names))
    exists <- file.exists(fullnames)
    matched <- c(matched, normalizePath(fullnames[exists], winslash = "/"))
    names <- names[!exists]
    if (!length(names)) break
  }
  if (length(names)) 
    warning("file(s) ", paste(names, collapse = " "), "not found.")
    
  names <- c(matched, names)
  return(names)
}

listRfiles <- function(prof = "Rprof.out", dirs = "."){
  if(is.character(prof)) {
    prof <- summaryRprof(prof, lines="show")
  }
  if(!is.list(prof) || !("by.line" %in% names(prof))){
    stop("'prof' should be a summaryRprof object with line profiling")
  }
  
  loc <- rownames(prof$by.line)
  names <- sub("#.*","",loc)
  names <- unique(names[names != "<no location>"])
  
  if(!length(names)) 
    warning("No R file locations found")
  else 
    names <- expandFilenames(names, dirs)
  names
}
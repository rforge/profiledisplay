profiler <- function(Rcode, dir = ".",pkg=NULL) {
    Rprof("Rprof.out", line.profiling=TRUE, memory.profiling=TRUE)
    source(file.path(dir,Rcode), echo=TRUE)
    Rprof(NULL)
    s <- summaryRprof("Rprof.out", lines="show", memory="both")
return(s)
}

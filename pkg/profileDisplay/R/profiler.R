profiler <- function(Rcode) {
    Rcode <- normalizePath(Rcode)
    Rprof( line.profiling=TRUE, memory.profiling=TRUE)
    source(Rcode, echo=TRUE)
    Rprof(NULL)
    s <- summaryRprof("Rprof.out", lines="show", memory="both")          
return(s)
}

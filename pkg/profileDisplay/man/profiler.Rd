\name{profiler}
\alias{profiler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Profiles the R code/Rprof data for \code{\link{profileHTML}} to highlight.
}
\description{
Takes the R code/Rprof data, profiles it and allows \code{\link{profileHTML}} to highlight.
}
\usage{
profiler(Rprof="Rprof.out", Rcode=NULL, newpath, profileType=c("self","total", "memory"))
}
\arguments{
  \item{Rprof}{
Default arguement is set to take in summary Rprof data with line profiling data in it within the current directory.  Can change the "Rprof" name to different profiling data if in different location and name.
}  
\item{Rcode}{
If file does not have line profiling data in it, the code is profilied and the data is saved to an "Rprof.out" and the data is used to profile.
}
  \item{newpath}{
Location of the file save in a directory.
}  
  \item{profileType}{
Option to look at profile data using "self" or "total" times, or memory usage.
}
}
\value{
Returns the summaryRprof data for the R code/Rprof data entered.
}
\author{
Name
}
\examples{


}

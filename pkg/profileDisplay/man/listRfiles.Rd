\name{listRfiles}
\alias{listRfiles}
\title{
List the executed files.
}
\description{
This function list the executed R files with the running times in \code{\link{summaryRprof}} output with lines="show".
}
\usage{
listRfiles(s = "Rprof.out")
}
\arguments{
  \item{s}{
output from \code{\link{summaryRprof}}.
}
}
\details{
s should be a profile object with line profiling.
}
\value{
This function returns a vector containing the names of the executed Rfiles.
}
\author{
Yuying Xie
}
\seealso{
\code{\link{summaryRprof}}
}
\examples{
library(profileDisplay)
tools:::.createExdotR("tables", system.file(package="tables"))
lines<-readLines("tables-Ex.R")
lines<-lines[!grepl("quit\\\('no'\\\)",lines)]
writeLines(lines,con="tables-Ex.R")
Rprof(line.profiling=TRUE)
source("tables-Ex.R")
Rprof(NULL)
pkg<-"tables"
s <- summaryRprof("Rprof.out", lines="show")
listRfiles(s)
}


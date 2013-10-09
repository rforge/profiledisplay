\name{findblocks}
\alias{findblocks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds and classifies the blocks of the codes.
}
\description{
This function finds the blocks of the codes and based classifies those blocks into differernt floors on the \code{\link{parse}} and \code{\link{getParseData}}.
}
\usage{
findblocks(s = NULL, filename = NULL)
}
\arguments{
  \item{s}{
Default argument is NULL, the input should be the \code{\link{summaryRprof}} data.
}
  \item{filename}{
Default is NULL, the input should be the name of the file we need to get the parse data and display the file.
}
}
\details{

}
\value{
Returns a list of vectors which includes related information about blocks, parsedata and lines of codes that have been executed.
}
\note{
Function works within the \code{\link{profileClassifier}}
}
\author{
Yuying Xie
}
\seealso{
\code{\link{getParseData}}
}
\examples{
library(profileDisplay)
tools:::.createExdotR("tables", system.file(package="tables"))
lines<-readLines("tables-Ex.R")
lines<-lines[!grepl("quit\\('no'\\)",lines)]
writeLines(lines,con="tables-Ex.R")
pkg<-"tables"
Rprof(line.profiling=TRUE)
source("tables-Ex.R")
Rprof(NULL)
s <- summaryRprof("Rprof.out", lines="show");filename<-"tabular.R";newR<-getwd()
findblocks(s,filename)
}
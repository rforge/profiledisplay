\name{findMultiblocks}
\alias{findMultiblocks}

\title{
Find multilines blocks
}
\description{
This function finds the blocks that takes more than 1 line according to the given code file.
}
\usage{
findMultiblocks(s = NULL, d, filename = NULL)
}

\arguments{
  \item{s}{
profile data from \code{\link{summaryRprof}} for the sourced codes.
}
  \item{d}{
parse data of the codes.
}
  \item{filename}{
The file path of the codes including the file name and directory.
}
}

\value{
A list with three components for the code file will be returned.
\item{uline}{The subset from parse data indicating the location and range of multilines blocks.}
\item{A character vector indicating the classification of each lines, type 1,2,3 and 4.}
\item{ln1}{The executed line numbers showing in profile data.}
}

\author{
Yuying Xie
}

\examples{
code<-readLines(system.file("/text/Example1.R",package="profileDisplay"))
p<-parse(text=code)
d<-getParseData(p)#create the parse data
data(lineSummary)
findMultiblocks(s=lineSummary,d=d,filename="Example1.R")
}


\name{coverageClassifier}
\alias{coverageClassifier}
\title{
Classify the lines of code file according to the executed lines
}
\description{
This is the main function to classify the codes into four categories based on executed lines of codes.
}
\usage{
coverageClassifier(prof, filename, d, legend = TRUE)
}
\arguments{
  \item{prof}{
profile data from \code{\link{summaryRprof}}.
}
  \item{filename}{
The file path of the codes including the file name and directory.
}
  \item{d}{
parse data of the codes.
}
  \item{legend}{
TRUE if we want to return the labels for each category of lines.
}
}

\value{
A list with three or four components will be returned.

\item{filename}{file path of the R codes.}
\item{class}{A character vector indicating the classification of each lines, type 1,2,3 and 4.}
\item{color}{The four colors for different categories of lines.}
\item{labels}{label of description for each category.}
}

\author{
Yuying Xie
}

\seealso{
\code{\link{summaryRprof}},\code{\link{findMultiblocks}}
}
\examples{
code<-readLines(system.file("/text/Example1.R",package="profileDisplay"))
p<-parse(text=code)
d<-getParseData(p)#create the parse data
data(lineSummary)
coverageClassifier(lineSummary,filename="~/inst/text/Example1.R",d=d,legend=TRUE)
}

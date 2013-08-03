\name{linenumbers}
\alias{linenumbers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Line number generator
}
\description{
Finds the line numbers and total line numbers from the highlight data for each level of runtime.
}
\usage{
linenumbers(htmldata, int=12)
}
\arguments{
  \item{htmldata}{
output of the "highlight" function code under the \code{\link{newdetective}} detective
}
  \item{int}{
Number of intervals needed for the profile.  Must be at least 3 intervals specified.
}
}
\value{
Returns a list of two values.  Measures is the list of line numbers for each class of time sorted by the New detective.  Numericals is a list of the total number of lines for each class of time sorted by the New detective.
}
\author{
Name
}
\note{
Function works within the \code{\link{profileHTML}} function.
}
\seealso{
\code{\link{newdetective}}, \code{\link{profileHTML}}
}
\examples{
linenumbers(readLines(system.file("/text/temp.txt", package = "profileDisplay"))) # two lists formed from highlight data with newdetective
linenumbers(readLines(system.file("/text/temp.txt", package = "profileDisplay")), int=5) #ifferent number of intervals
}
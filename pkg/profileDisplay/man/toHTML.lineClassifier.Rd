\name{toHTML.lineClassifier}
\alias{toHTML.lineClassifier}
\title{
toHTML method for lineClassifier objects
}
\description{
This converts a \code{"lineClassifier"} object into a 
collection of HTML files.
}
\usage{
\method{toHTML}{lineClassifier}(x, htmldir = tempdir(), ...)
}
\arguments{
  \item{x}{
A line classifier object.
}
  \item{htmldir}{
The output directory.
}
  \item{...}{
Parameters passed to \code{\link{writeCSS}}.
}
}
\value{
This returns the name of the output directory, e.g. to
use in \code{\link{browseURL}}.
}
\author{
Duncan Murdoch and Alexander Tsang
}
\seealso{
\code{\link{selfprofileClassifier}}
}
\examples{
data(lineSummary)
lines <- selfprofileClassifier(lineSummary, dir = system.file("text", package = "profileDisplay"))
browseURL( toHTML(lines) )
}

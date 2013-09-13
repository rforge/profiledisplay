\name{barview}
\alias{barview}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates an optional sidebar in \code{\link{profileHTML}}.
}
\description{
Takes in the HTML produced in \code{\link{profileHTML}} and adds a bar chart to the side if requested.
}
\usage{
barview(int, htmldata)
}
\arguments{
  \item{int}{
Number of intervals needed for the profile.  Must be at least 3 intervals specified.
}
  \item{htmldata}{
output of the "highlight" function code under the \code{\link{newdetective}} detective
}
}
\value{
Returns a bar chart on the side of the Code section of the HTML page.
}
\author{
Name
}
\examples{


}

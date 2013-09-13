\name{classdeterminer}
\alias{classdeterminer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds heavy usage lines for \code{\link{profileHTML}}.
}
\description{
Determines which lines of the R code were sorted into the highest class by \code{\link{newdetective}}.
}
\usage{
classdeterminer(colourer,measure,int, name,profileType, datavals)
}
\arguments{
	  \item{colourer}{
Initial HTML input.
}
  \item{measure}{
Data received after HTML is passed through \code{\link{linenumbers}}.
}
  \item{int}{
Number of intervals needed for the profile.  Must be at least 3 intervals specified.
}
  \item{name}{
The total list of parse data names produced by the file.
}
  \item{profileType}{
Option to look at profile data using "self" or "total" times, or memory usage.
}
  \item{datavals}{
Parse data received from \code{\link{newdetective}}
}
}
\value{
Returns a vector of HTML listing where heavy usage lines are and how long they take.
}
\author{
Name
}
\examples{


}

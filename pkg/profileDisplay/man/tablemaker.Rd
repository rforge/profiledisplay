\name{tablemaker}
\alias{tablemaker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates the code to produce the table for the legend in \code{\link{profileHTML}}.
}
\description{
Takes in the HTML produced in \code{\link{profileHTML}} and adds a legend.
}
\usage{
tablemaker(names,values,int=12, timeintervals, profileType, measure)
}
\arguments{
	  \item{names}{
The total list of parse data names produced by the file.
}
  \item{values}{
The times organized alphabetically by the file it is from and then put in chronological order by line number.
}
  \item{int}{
Number of intervals needed for the profile.  Must be at least 3 intervals specified.
}
  \item{timeintervals}{
a vector of time intervals based on the "int" value
}
  \item{profileType}{
Option to look at profile data using "self" or "total" times, or memory usage.
}
  \item{measure}{
Data received after HTML is passed through \code{\link{linenumbers}}.
}
}
\value{
Returns a table in the Legend section of the HTML page.
}
\author{
Name
}
\examples{


}

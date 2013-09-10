\name{codecondenser}
\alias{codecondenser}
\title{
Code condenser
}
\description{
Takes the highlight code, edits the tokens to simplify the HTML code produced.
}
\usage{
codecondenser(htmldata, int=12)
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
Simplifies the code produced by the highlight function by combining similar class times in the same line together.
}
\author{
Name
}
\note{
Function works within the \code{\link{profileHTML}} function.
}
\seealso{
 \code{\link{profileHTML}},  \code{\link{highlight}}
}
\examples{
system.file("/text/temp.txt", package = "profileDisplay")
  profileDisplay:::codecondenser(readLines(system.file("/text/temp.txt", package = "profileDisplay"))) #simplifies the HTML so class is per line, not per token
system.file("/text/temp.txt", package = "profileDisplay")
  profileDisplay:::codecondenser(readLines(system.file("/text/temp.txt", package = "profileDisplay")), int=5) #different number of intervals
}


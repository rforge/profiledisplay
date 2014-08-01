\name{print.lineClassifier}
\alias{print.lineClassifier}
\title{
Print method for lineClassifier objects
}
\description{
This prints a lineClassifier objects.
}
\usage{
\method{print}{lineClassifier}(x, detail = FALSE, ...)
}
\arguments{
  \item{x}{
a line classifier object.
}
  \item{detail}{
logical; if TRUE, the detail of summaryRprof data will be printed.
}
  \item{...}{
other parameters to pass to \code{print}. 
}
}

\author{

}

\examples{
data(lineSummary)
result<-profileClassifier(lineSummary, dir = system.file("text", package = "profileDisplay"))
print.lineClassifier(result)
}

\name{print.lineClassifier}
\alias{print.lineClassifier}
\title{
Print method for lineClassifier objects
}
\description{
This prints a lineClassifier objects.
}
\usage{
\method{print}{lineClassifier}(x, ...)
}
\arguments{
  \item{x}{
a line classifier object.
}
  \item{detail}{
logical; if TRUE, the detail of summaryRprof data will be printed.
}

}

\author{

}

\examples{
data(lineSummary)
result<-selfprofileClassifier(lineSummary, dir = system.file("text", package = "profileDisplay"))
print.lineClassifier(result)
}

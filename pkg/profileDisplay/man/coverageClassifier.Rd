\name{coverageClassifier}
\alias{coverageClassifier}
\title{
Classify the lines of code file according to the executed lines
}
\description{
This is the main function to classify the codes into four categories based on executed lines of codes.
}
\usage{
coverageClassifier(profileType="Executed",prof="Rprof.out",dir=".")
}
\arguments{
  \item{profileType}{
  The type of the profile data file, the default is "Executed". The other profile types are "Self" , "Total" and "Memory".
}
  \item{dir}{
A directory where to look for the source files.
}

}

\value{
An object of class \code{"lineClassifer"}.  This is a list with four components.
The \code{files} component is
list of dataframes, one per source file.  The dataframes each contain the following columns:
\item{line1}{The line number.}
\item{times}{How much time was observed on that line.}
\item{styles}{Classification of this line.}
\item{lines}{The actual text for the lines.}

The \code{titles} component contains a text title for each file.

The \code{info} component contains extra information for each file, 
to be displayed just before the code.

The \code{summaryRprof} component contains a copy of the summaryRprof data.

}

\author{
Yuying Xie
}

\seealso{
\code{\link{summaryRprof}},\code{\link{toHTML.lineClassifier}}
}
\examples{
x<-coverageClassifier(profileType="Executed",lineSummary,dir=system.file("text",package="profileDisplay"))
toHTML.lineClassifier(x, profileType="Executed", htmldir = tempdir())
}

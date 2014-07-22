\name{selfprofileClassifier}
\alias{selfprofileClassifier}
\title{
Classify source lines according the self time in profile
}
\description{
This function classifies each line of source files according
to the time spent on that line in profile data.
}
\usage{
selfprofileClassifier(prof = "Rprof.out", 
                      dir = ".", 
		      src = listRfiles(prof, dir), 
		      breaks = 6,
		      colourdata = colouring()$colourdata)
}
\arguments{
  \item{prof}{
The name of a profile data file, or the result of \code{\link{summaryRprof}} on one.
}
  \item{dir}{
A vector of directories in which to look for the source files.
}
  \item{src}{
The file names to classify.
}
  \item{breaks}{
Breaks in values to be passed to \code{\link{cut}}.
}
  \item{colourdata}{
The colour scheme to use for the display.
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
Alexander Tsang and Duncan Murdoch
}
\examples{
data(lineSummary)
result <- selfprofileClassifier(lineSummary, dir = system.file("text", package = "profileDisplay"))
print.lineClassifier(result)
}
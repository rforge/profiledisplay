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
		      colourdata = colouring()$colourdata)
}
\arguments{
  \item{prof}{
The name of a profile data file, or the result of \code{\link{summaryRprof()}} on one.
}
  \item{dir}{
A vector of directories in which to look for the source files.
}
  \item{src}{
The file names to classify.
}
  \item{colourdata}{
The colour scheme to use for the display.
}
}
\value{
An object of class \code{"lineClassifer"}.  This is a list with three components.
The \code{files} component is
list of dataframes, one per source file.  The dataframes each contain the following columns:
\item{line1}{The line number.}
\item{times}{How much time was observed on that line.}
\item{styles}{Classification of this line.}
\item{line}{The actual text for the line.}

The \code{titles} component contains a text title for each file.

The \code{info} component contains extra information for each file, 
to be displayed just before the code.
}
\author{
Alexander Tsang and Duncan Murdoch
}
\examples{
data(lineSummary)
selfprofileClassifier(lineSummary, dir = system.file("text", package = "profileDisplay"))
}
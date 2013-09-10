\name{newdetective}
\alias{newdetective}
\title{
New detective
}
\description{
This detective uses parse data and with summaryRprof self time data by line to make its investigation.
}
\usage{
newdetective(x, value, ln, fn, names, rprofdata, type, int=12, datawant=FALSE,styleswant=TRUE)
}
\arguments{
  \item{x}{
The parse data produced by each file.
}
  \item{value}{
The times organized alphabetically by the file it is from and then put in chronological order by line number.
}
  \item{ln}{
The line number corresponding to the "values" time.
}
  \item{fn}{
The parse data names produced by the file.
}
  \item{names}{
The total list of parse data names produced by the file.
}
  \item{rprofdata}{
The data frame produced by summaryRprof is applied.
}
  \item{type}{
The type of profiling is asked for; self, total or memory.
}
  \item{int}{
The number of intervals needed to be produced in order to produce the correct number of classes to sort the times.  Must be at least 3.
}
  \item{datawant}{
Returns the data table produced.
}
  \item{styleswant}{
Returns the data character vector wanted.
}
}
\value{
Returns a character vector grouping similar lines together by run time if stylewant is TRUE.  Returns a data frame of parse data and times if datawant is TRUE.
}
\note{The \samp{"newdetective"} stylesheet is works within the \code{\link{profileHTML}} function}
\author{
Name
}
\seealso{
\code{\link{simple_detective}}
}

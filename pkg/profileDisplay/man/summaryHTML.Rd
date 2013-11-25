\name{summaryToHTML}
\alias{summaryToHTML}
\title{
Display raw \code{\link{summaryRrof}} output as HTML.
}
\description{
This outputs \code{\link{summaryRprof}} as HTML.
}
\usage{
summaryToHTML(prof = "Rprof.out", 
            show = c("self", "total", "memory"), 
	    dir = tempdir(),
	    outfile = "summaryRprof.html")
}
\arguments{
  \item{prof}{
Either the filename containing profile data, or a \code{\link{summaryRprof}}
object.
}
  \item{show}{
Which type of data to show if the summary needs to be produced.
}
  \item{dir}{
The directory in which to write the results.
}
  \item{outfile}{
The filename to write to.
}
}
\value{
The name of the file that was written.
}

\author{
Alexander Tsang and Duncan Murdoch
}

\examples{
  # Read a prepared profile summary
  data(lineSummary, package = "profileDisplay")
  
  # Display it
  browseURL(summaryHTML(lineSummary))
}

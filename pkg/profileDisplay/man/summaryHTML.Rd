\name{summaryHTML}
\alias{summaryHTML}
\title{
Display raw \code{\link{summaryRrof}} output as HTML.
}
\description{
This outputs \code{\link{summaryRrof}} as HTML.
}
\usage{
summaryHTML(prof = "Rprof.out", 
            show = c("self", "total", "memory"), 
	    dir = tempdir())
}
\arguments{
  \item{prof}{
Either the filename containing profile data, or a \code{\link{summaryRprof}}
object.
}
  \item{show}{
Which type of data to show.
}
  \item{dir}{
The directory in which to write the results.
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

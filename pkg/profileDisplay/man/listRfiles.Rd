\name{listRfiles}
\alias{listRfiles}
\title{
List the files mentioned in a profile.
}
\description{
This function lists the files mentioned in a \code{\link{summaryRprof}} object.
Directories are searched to find the existing files.
}
\usage{
listRfiles(prof = "Rprof.out", dirs = ".")
}
\arguments{
  \item{prof}{
The filename of the output of \code{\link{Rprof}}, or an object resulting
from \code{\link{summaryRprof}}.
}
  \item{dirs}{
A vector of directory names in which to look for the files.
}
}
\details{
Line profiling needs to be enabled in the profile data and in the 
summary data.
}
\value{
This function returns a vector containing the names of the files.  If found
in the specified directories, the filenames will be fully expanded; if not
found, the basename of the file will be returned with a warning.
}
\author{
Yuying Xie and Duncan Murdoch
}
\seealso{
\code{\link{Rprof}}, \code{\link{summaryRprof}}
}
\examples{
  # Read a prepared profile summary
  data(lineSummary, package = "profileDisplay")
 
  listRfiles(lineSummary, dirs = system.file("text", package = "profileDisplay"))
}


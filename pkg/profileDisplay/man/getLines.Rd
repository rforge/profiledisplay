\name{getLines}
\alias{getLines}
\title{
Get the source code lines from named files.
}
\description{
This function retrieves the source code file files as named
in \code{\link{Rprof}} output.
}
\usage{
getLines(filename, dir = ".", pkg = NULL)
}
\arguments{
  \item{filename}{
One or more filenames to retrieve.
}
  \item{dir}{
The directory in which to look.
}
  \item{pkg}{
Either a package name or a source tarball name for a package
}
}
\details{
If \code{pkg} is not specified, the function looks for the files
in the directory named by \code{dir}.

A package may be named in two ways.  If \code{pkg} is given
as the name of a \file{.tar.gz} file (the source tarball for
a package), the tarball is expanded and the files are sought
within the \file{R} subdirectory.  If \code{pkg} does not
appear to be of that form, then it is assumed to name a
downloadable package; the package tarball is downloaded
and that's where the source is sought.
}
\value{
A named list of character vectors, one per \code{filename}.  
These are the lines of the corresponding source file.
}
\author{
Duncan Murdoch
}
\examples{
\dontrun{
getLines("getLines.R", pkg = "profileDisplay")
}
}

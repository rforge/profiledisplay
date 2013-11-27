\name{expandGroups}
\alias{expandGroups}
\title{
Expand group designation to other lines in multi-line statements.
}
\description{
If a statement is broken across several lines, the profiler will only
report the first line.  This function parses the source and
identifies continuation lines
that contain code, not only comments, and extends the group classification
to cover them as well.
}
\usage{
expandGroups(gp, lines)
}
\arguments{
  \item{gp}{
Initial classification for each line.
}
  \item{lines}{
The source code lines in a character vector.
}
}
\value{
A vector of the same size and type as \code{gp} with the typing extended.
}
\author{
Duncan Murdoch
}
\examples{
lines <- 
" x <- 1 +
  2 
  # 3
  # 4
  y <- 5
  z <- 6"
gp <- 1:6
expandGroups(gp, lines)
}

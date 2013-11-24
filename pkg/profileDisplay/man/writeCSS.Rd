\name{writeCSS}
\alias{writeCSS}
\title{
Write CSS file
}
\description{
This writes a CSS file to be us\name{writeCSS}
}
\usage{
writeCSS(classes, filename, bg = c("white", "#FED98E"), fg = c("black", "black"))
}
\arguments{
  \item{classes}{
A list of CSS class names.
}
  \item{filename}{
The file to which to write.
}
  \item{bg, fg}{
The range of background and foreground colours, from the first class to the last class.
}
}
\details{
This writes out evenly spaced colours from the first to last for each of the
named classes using \code{\link{colorRamp}}.
}
\value{
The filename.
}
\author{
Alexander Tsang and Duncan Murdoch
}
\examples{
  f <- tempfile(fileext = ".css")
  writeCSS(letters, f)
  readLines(f)
}

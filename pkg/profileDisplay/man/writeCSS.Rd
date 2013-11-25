\name{writeCSS}
\alias{writeCSS}
\title{
Write CSS file
}
\description{
This writes a CSS file to be used in the profile files.
}
\usage{
writeCSS(classes, filename, bg = c("white", "#FED98E"), fg = c("black", "black"),
         border.bg = "#FED98E")
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
  \item{border.bg}{
The background colour to use on the borders of the page.
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

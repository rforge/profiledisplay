\name{colouring}
\alias{colouring}

\title{
Matches a colour scheme to the new detective parse data
}
\description{
Takes a character vector of colours as input and matches it to the classes produced by the new detective.  Writes a new CSS file to use as colour scheme for the profileHTML highlighting data.
}
\usage{
colouring(colourdata = rgb(colorRamp(c("#FFFFD4", "#FED98E"), space = "Lab")(0:(int - 
          1)/(int - 1)), maxColorValue = 255), int=12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colourdata}{
The character vector of colours. Default colours are selected if customization is not desired.
}
  \item{int}{
Number of intervals needed for the profile.  Must be at least 3 intervals specified.
}
}
\value{
A CSS stylesheet "test.css" is produced and is saved to the working directory.
}
\author{
Name
}
\note{
Function works within the \code{\link{profileHTML}} function.
}
\seealso{
\code{\link{newdetective}}, \code{\link{profileHTML}}
}
\examples{
  profileDisplay:::colouring() #produces a CSS file with default colours for each class
  profileDisplay:::colouring(int=6) #CSS file with 6 colours
colours <- c("#FFA57F", "#FFA54E", "#FFA523", "#FFA500", "#EE9AEE", "#EE9AD8", "#EE9AAA", "#EE9A9A", "#EE9A77", "#EE9A49", "#EE9A21", "#EE9A00") #colours chosen by the user
  profileDisplay:::colouring(colours) #CSS file now with colours chosen by the user}
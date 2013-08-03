\name{profileDisplay-package}
\alias{profileDisplay-package}
\alias{profileDisplay}
\docType{package}
\title{
Highlights tokens from R profiling data based on runtime 
}
\description{
Function to highlight lines of R code based on runtime in the Rprof function.
}
\details{
\tabular{ll}{
Package: \tab profileDisplay\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-05-31\cr
}

}
\author{
Name
}
\seealso{
The main function of the package is \code{\link{profileHTML}}.
\code{\link{profileHTML}} highlights the tokens in the R code based on run time delagated by \code{\link{newdetective}} and produces a HTML page containing the profile information highlighted according to run time.  \code{\link{colouring}} creates the CSS stylesheet for each class of \code{\link{newdetective}}, and links a colour pallette to it.
To simplify the \code{\link{highlight}} code \code{\link{codecondenser}} puts a class per line based on runtime.  \code{\link{linenumbers}} gets takes the line numbers and total number of lines under given classes.

}
\examples{
profileHTML(Rcode=system.file("/text/example.R", package = "profileDisplay"),profileType="self", savefile=file.path(tempdir(),"temp2")) #returns a HTML page in browser.
colours <- c("#FFA57F", "#FFA54E", "#FFA523", "#FFA500", "#EE9AEE", "#EE9AD8", "#EE9AAA", "#EE9A9A", "#EE9A77", "#EE9A49", "#EE9A21", "#EE9A00") #colours chosen by the user
profileHTML(Rcode=system.file("/text/example.R", package = "profileDisplay"),profileType="self", colourdata= colours,savefile=file.path(tempdir(),"temp4")) #custom colour scheme for the HTML webpage
}

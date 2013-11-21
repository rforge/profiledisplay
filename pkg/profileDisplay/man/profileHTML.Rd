\name{profileHTML}
\alias{profileHTML}
\title{
Highlights the tokens
}
\description{
Highlights the lines of code based on runtime in \code{\link{newdetective}} classes and presents the data in an HTML page.
}
\usage{
profileHTML(Rprof="Rprof.out", Rcode=NULL, detective = newdetective, int=12, colourdata=
             rgb(colorRamp(c("#FFFFD4" ,"#FED98E"), space="Lab")(0:11/11), maxColorValue=255), 
           profileType=c("self","total", "memory"), openbrowser=TRUE, savefile=tempdir(),
           visual="all")
}
\arguments{
  \item{Rprof}{
Default arguement is set to take in summary Rprof data with line profiling data in it within the current directory.  Can change the "Rprof" name to different profiling data if in different location and name.
}  
\item{Rcode}{
If file does not have line profiling data in it, the code is profilied and the data is saved to an "Rprof.out" and the data is used to profile.
}
  \item{detective}{
The detective used to highlight the tokens under the \code{\link{highlight}} function.  The default detective is \code{\link{newdetective}}, which splits the run times into a specified number of different classes by "int".
}
  \item{int}{
Number of intervals needed for the profile.  Must be at least 3 intervals specified.
}
  \item{colourdata}{
The character vector of a specified number of colours set by "int". Default colours are selected if customization is not desired.  Data used in the \code{\link{colouring}} function to match the colours to the \code{\link{newdetective}} classes and produces a CSS stylesheet.  If "int" and the length of "colourdata" differ, the minimum number of intervals it taken from the two values.
}
  \item{profileType}{
Option to look at profile data using "self" or "total" times, or memory usage.
}
  \item{openbrowser}{
Option to open the HTML page in a browser window.
}
  \item{savefile}{
Option to save the HTML file to a folder directory.
}
  \item{visual}{
Chooses which information to display in HTML.  Default is set to all to show all infomation but using "code" only displays the code, "legend" only displays the legend, "info" only displays the first setence information, and "summary" returns only the summaryRprof data.
}
}
\value{
Returns an HTML page in your browser containing a table of contents, listing the code, a legend, heavy usage lines, and the summaryRprof data produced.
}
\author{
Name
}
\seealso{
\code{\link{highlight}}
}
\examples{
profileHTML(Rcode=system.file("/text/Example1.R", package = "profileDisplay"),profileType="self", savefile=file.path(tempdir(),"temp")) #returns a HTML page in browser.  Notice heavy usage in the for loop.
colours <- c("#FFA57F", "#FFA54E", "#FFA523", "#FFA500", "#EE9AEE", "#EE9AD8", "#EE9AAA", "#EE9A9A", "#EE9A77", "#EE9A49", "#EE9A21", "#EE9A00") #colours chosen by the user
profileHTML(Rcode=system.file("/text/Example2.R", package = "profileDisplay"),profileType="self", colourdata= colours,savefile=file.path(tempdir(),"temp1")) #custom colour scheme for the HTML webpage. Time changes now do to removal of addition to a vector.
profileHTML(Rcode=system.file("/text/Example3.R", package = "profileDisplay"),profileType="total", savefile=file.path(tempdir(),"temp1")) #using total time #NW
profileHTML(Rcode=system.file("/text/Example4.R", package = "profileDisplay"),profileType="memory",savefile=file.path(tempdir(),"temp1")) #memory usage tested #NW
profileHTML(Rcode=system.file("/text/ExampleR.R", package = "profileDisplay"),profileType="self",savefile=file.path(tempdir(),"temp1")) #multiple files pulled
}

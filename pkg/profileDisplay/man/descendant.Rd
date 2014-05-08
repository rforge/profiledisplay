\name{descendant}
\alias{descendant}
\title{
Get the parse data of block or statement.
}
\description{
This function will retrirve the parse data related to the specified id.
}
\usage{
descendant(data = NULL, id = NULL)
}
\arguments{
  \item{data}{
The output from the \code{\link{getParseData}}, \code{\link{parse}}.
}
  \item{id}{
The id for each line in parse data.
}
}
\details{
Given an id from parse data, the function can retrieve the parse data having the same parent, whose parents'(grandparents, great grantparents and so on) id are the same. 
}
\value{
A data.frame as a subset of parse data that all parse data inside the same block or statement will be returned.
}
\author{
Yuying Xie
}
\seealso{
\code{\link{getParseData}}, \code{\link{parse}}
}
\examples{
#original code
set.seed(20130821)
X <- runif(100000)
Y <- runif(100000)

#Initially test by adding length to a vector
Z <- numeric()
add1 <- function(X,Y){
  for (i in 1:max(length(X),length(Y))) {
    Zold <- X[i] + Y[i]
    Z <- c(Z,Zold)
  }
  return(Z)
}


code<-readLines(system.file("/text/Example1.R",package="profileDisplay"))
p<-parse(text=code)
d<-getParseData(p)#create the parse data
d[d$id \%in\% descendant(data=d,id=140),"text"]#140 is id of the statement: Zold<-X[i] +Y[i]
}

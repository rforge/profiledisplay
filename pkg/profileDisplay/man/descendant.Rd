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
Given an id from parse data, the function can retrieve the parse data having the same parent, whose parents(grandparents, great grantparents and so on) id are the same. 
}
\value{
A list as a subset of parse data that all parse data inside the same block or statement will be returned.
}
\author{
Yuying Xie
}
\seealso{
\code{\link{getParseData}}, \code{\link{parse}}
}
\examples{
Eratosthenes <- function(n) {#create fucntion
  # Return all prime numbers up to n
  # (based on the sieve of Eratosthenes
  if (n >= 2) {
    sieve <- seq(2, n)
    primes <- c()
    for (i in seq(2, n)) {
      if (any(sieve == i)) {
        primes <- c(primes, i)
        sieve <- sieve[(sieve \%\% i) != 0]
      }
    }
    return(primes)
  } else {
    stop("Input value of n should be at least 2.")
  }
}
dump("Eratosthenes")#save it into dumpdata.R
p<-parse("dumpdata.R")
d<-getParseData(p)#create the parse data
d[d$id \%in\% descendant(data = d, id = sample(d$id,1)),"text"]
}

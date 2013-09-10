\name{missingseq}
\alias{missingseq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a list of numbers missing in a sequence
}
\description{
Given a numeric vector, finds the range and determines which integer values are not included between the vector
}
\usage{
missingseq(x)
}
\arguments{
  \item{x}{
The numeric vector of values.
}
}
\value{
Returns a numeric vector of integers not included in the sequence between the range of the values.
}
\author{
Name
}
\examples{
x <- c(6,2,9,3,7)
  profileDisplay:::missingseq(x) #range is from 2 to 9, thus returning 4,5,8
}

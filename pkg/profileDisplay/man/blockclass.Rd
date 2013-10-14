\name{blockclass}
\alias{blockclass}
\title{
Find the class for each block.
}
\description{
Classifiy those blocks into different class using recursive method. Blocks might contains some chindren classes and also might belongs to some parent classes.
}
\usage{
blockclass(x, i)
}
\arguments{
  \item{x}{
x is the modified output from  \code{\link{getParseData}}.
}
  \item{i}{
i starts from 0 and it represents which floor/class the block belongs to.
}
}
\details{

}
\value{
Return the data.frame which containg both the modified output from \code{\link{getParseData}} and floor information which indicates which floor each block belongs to.
}

\author{
Yuying Xie
}
\seealso{
\code{\link{getParseData}},\code{\link{findblocks}}
}
\examples{

}

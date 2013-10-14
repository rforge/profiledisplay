\name{createEx}
\alias{createEx}
\title{
Create example file.
}
\description{
This function creates the example file from the selected package.
}
\usage{
createEx(pkg = NULL)
}
\arguments{
  \item{pkg}{
The selected package names to get the example file.
}
}
\details{
If pkg is not specified, an error will bw returned.
}
\value{
The example file will ve created,a character vector containing the directory information of the created R example file will be returned.
}
\author{
Yuying Xie
}
\examples{
createEx("tables")
}


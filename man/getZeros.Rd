\name{getZeros}
\alias{getZeros}
\title{Find Zero Crossing Points}
\usage{
getZeros(x, len = 3)
}
\arguments{
  \item{x}{numeric vector}

  \item{len}{natural number window length for sign
  comparison}
}
\value{
logical vector of length zero
}
\description{
Find positions in a numeric vector where sign has changed
on average for more than n elements
}
\examples{
x1 <- -7:8
   getZeros(x = x1)
   x2 <- rep(-1:1, times = 6)
   getZeros(x = x2)
}


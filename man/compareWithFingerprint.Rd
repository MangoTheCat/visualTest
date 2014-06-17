\name{compareWithFingerprint}
\alias{compareWithFingerprint}
\title{compare a test fingerprint with a known fingerprint}
\usage{
compareWithFingerprint(test, fingerprint, threshold = 0.001, exact = FALSE)
}
\arguments{
  \item{test}{numeric vector}

  \item{fingerprint}{numeric vector}

  \item{threshold}{single numeric similarity parameter
  (default 1e-3)}

  \item{exact}{single logical should fingerprints match
  exactly (default \code{FALSE})}
}
\value{
single logical
}
\description{
compare a test fingerprint with a known fingerprint
}
\examples{
compareWithFingerprint(test = 1:3, fingerprint = 1:3)
    compareWithFingerprint(test = 1:3, fingerprint = 1:4)
    compareWithFingerprint(test = 1:3, fingerprint = 1:3 + 1e-3)
    compareWithFingerprint(test = 1:3, fingerprint = c(1, 2, 3.1))
    compareWithFingerprint(test = 1:3, fingerprint = c(1, 2, 3.1), exact = TRUE)
}


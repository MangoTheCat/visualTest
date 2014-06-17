\name{getFingerprint}
\alias{getFingerprint}
\title{Get Image Fingerprint}
\usage{
getFingerprint(file, ...)
}
\arguments{
  \item{file}{single character naming PNG or JPG file from
  which to get fingerprint}

  \item{\dots}{additional arguments}
}
\description{
Get Image Fingerprint
}
\examples{
getFingerprint(file.path(system.file(package = "visualTest"), "compare", "stest-00.jpg"))
}


\name{showFingerprint}
\alias{showFingerprint}
\title{Show Fingerprint(s)}
\usage{
showFingerprint(...)
}
\arguments{
  \item{\dots}{files to test}
}
\value{
list of fingerprint(s) invisibly.  As a side effect, a plot
of fingerprints is created.
}
\description{
Show Fingerprint(s)
}
\examples{
sf <- system.file(package = "visualTest")
    eg <- "VR-616_plot-lm00.jpg"
    showFingerprint(file.path(sf, "compare", "windows", eg),
          file.path(sf, "compare", "unix", eg))
}


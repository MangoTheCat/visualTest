
#' A function to check similarity between plots. By default, fingerprint
#' comparison is very fuzzy, and the fuzziness can be controlled by
#' argument threshold (see \code{\link{compareWithFingerprint}}).
#' Fingerprint exact matching can be selected with argument exact. Note
#' that similar looking images may have the same fingerprint (see
#' \code{\link{getFingerprint}}).
#'
#' @title Is this Graphic a Match to this Fingerprint?
#' @param file single character naming PNG or JPG file from which to get
#'   fingerprint
#' @param fingerprint fingerprint, or filename. Character scalars
#'   without a dot in them and numeric vectors are taken as fingerprints.
#' @param threshold single numeric similarity parameter (default 1e-3)
#' @param exact single logical should fingerprints match exactly (default
#'   \code{FALSE}). Note that for the \code{original} algorithm, this
#'   is not equivalent to using a zero threshold.
#' @param ... additional arguments
#'
#' @export
#' @examples
#' rdata <- matrix(rnorm(200), ncol = 2)
#'
#' ## To create the images in a temporary directory
#' tmp <- function(path) file.path(tempdir(), path)
#'
#' png(tmp("test1.png"))
#' plot(rdata)
#' dev.off()
#'
#' fing1 <- getFingerprint(file = tmp("test1.png"))
#' rdata[2, 2] <- 0.1
#' png(tmp("test2.png"))
#' plot(rdata)
#' dev.off()
#'
#' isSimilar(file = tmp("test2.png"), fingerprint = fing1)
#' isSimilar(file = tmp("test2.png"), fingerprint = fing1, threshold = 0.05)
#'
#' png(tmp("test3.png"))
#' plot(rdata, col = 3)
#' dev.off()
#'
#' isSimilar(file = tmp("test3.png"), fingerprint = fing1)
#' isSimilar(file = tmp("test3.png"), fingerprint = fing1, threshold = 0.05)
#'
#' rdata2 <- matrix(rnorm(200), ncol = 2)
#' png(tmp("test4.png"))
#' plot(rdata2)
#' dev.off()
#'
#' isSimilar(file = tmp("test4.png"), fingerprint = fing1)
#' isSimilar(file = tmp("test4.png"), fingerprint = fing1, threshold = 5)
#'
#' png(tmp("test5.png"))
#' hist(rdata2)
#' dev.off()
#'
#' isSimilar(file = tmp("test5.png"), fingerprint = fing1)
#' isSimilar(file = tmp("test5.png"), fingerprint = fing1, threshold = 5e6)

isSimilar <- function(file, fingerprint, threshold = 1e-3,
                      exact = FALSE, ...) {

  if (missing(file)) { stop("file is missing") }
  if (missing(fingerprint)) { stop("fingerprint is missing") }
  if (!is.character(file) || length(file) != 1) {
    stop("file should be a single character")
  }
  if (!file.exists(file)) stop("file does not exist")

  if (is.numeric(fingerprint) ||
      (is.character(fingerprint) && length(fingerprint) == 1)) {
    if (is.character(fingerprint) && length(fingerprint) == 1 &&
        grepl(".", fingerprint, fixed = TRUE)) {
      fingerprint <- getFingerprint(file = fingerprint, ...)
    }

  } else {
    stop("fingerprint should be a numeric vector or a single character")
  }

  test <- getFingerprint(file = file, ...)

  compareWithFingerprint(
    test = test,
    fingerprint = fingerprint,
    threshold = threshold,
    exact = exact
  )
}


#' Compare a test fingerprint with a known fingerprint
#'
#' @param test fingerprint
#' @param fingerprint fingerprint
#' @param threshold single numeric similarity parameter,
#'   for the \code{original} algorithm it defaults to 1e-3.
#'   For the \code{dct} method it is the number bits allowed to
#'   be different in the fingerprint (i.e. Hamming distance).
#' @param exact single logical should fingerprints match exactly (default
#'   \code{FALSE})
#' @return single logical
#'
#' @export
#' @importFrom stats fivenum
#' @examples
#' compareWithFingerprint(test = 1:3, fingerprint = 1:3)
#' compareWithFingerprint(test = 1:3, fingerprint = 1:4)
#' compareWithFingerprint(test = 1:3, fingerprint = 1:3 + 1e-3)
#' compareWithFingerprint(test = 1:3, fingerprint = c(1, 2, 3.1))
#' compareWithFingerprint(
#'   test = 1:3,
#'   fingerprint = c(1, 2, 3.1),
#'   exact = TRUE
#' )

compareWithFingerprint <- function(test, fingerprint, threshold = 1e-3,
                                   exact = FALSE) {

  if (is.numeric(test) + is.numeric(fingerprint) == 1) {
    stop("Cannot compare fingerprints from different algorithms")
  }

  if (is.numeric(test)) {
    compareWithFingerprintOriginal(test, fingerprint, threshold, exact)

  } else {
    compareWithFingerprintDCT(test, fingerprint, threshold, exact)
  }
}

compareWithFingerprintOriginal <- function(test, fingerprint, threshold, exact) {
  if (exact) {
    if (length(test) == length(fingerprint)) {
      if (all(test == fingerprint)) return(TRUE)
    }

  } else {
    if (all(abs(fivenum(test) - fivenum(fingerprint)) <= abs(threshold))) {
      return(TRUE)
    }
  }

  FALSE
}

compareWithFingerprintDCT <- function(test, fingerprint, threshold, exact) {
  if (exact) threshold <- 0
  hamming_distance(test, fingerprint) <= threshold
}

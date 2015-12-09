
#' Use \pkg{visualTest} to perform fuzzy image matching
#' 
#' @author Chris Campbell \email{ccampbell@@mango-solutions.com}
#' @name visualTest-package
#' @aliases visualTest
#' @docType package
#' @title Extract and match characteristic signal in graphs
#' @import png jpeg bmp
NULL


#' A function to check similarity between plots. By default, fingerprint comparison is very fuzzy, 
#' and the fuzziness can be controlled by argument threshold (see \code{\link{compareWithFingerprint}}).
#' Fingerprint exact matching can be selected with argument exact. Note that similar looking images may 
#' have the same fingerprint (see \code{\link{getFingerprint}}).
#' 
#' @title Is this Graphic a Match to this Fingerprint?
#' @param file single character naming PNG or JPG file from which to get fingerprint
#' @param fingerprint numeric vector identifying file expected, or a single character naming a file to fingerprint
#' @param threshold single numeric similarity parameter (default 1e-3)
#' @param exact single logical should fingerprints match exactly (default \code{FALSE})
#' @param ... additional arguments
#'
#' @export
#' @examples
#' rdata <- matrix(rnorm(200), ncol = 2)
#'
#' png("test1.png")
#' plot(rdata)
#' dev.off()
#'
#' fing1 <- getFingerprint(file = "test1.png")
#' rdata[2, 2] <- 0.1
#' png("test2.png")
#' plot(rdata)
#' dev.off()
#' 
#' isSimilar(file = "test2.png", fingerprint = fing1)
#' isSimilar(file = "test2.png", fingerprint = fing1, threshold = 0.05)
#'
#' png("test3.png")
#' plot(rdata, col = 3)
#' dev.off()
#' 
#' isSimilar(file = "test3.png", fingerprint = fing1)
#' isSimilar(file = "test3.png", fingerprint = fing1, threshold = 0.05)
#'
#' rdata2 <- matrix(rnorm(200), ncol = 2)
#' png("test4.png")
#' plot(rdata2)
#' dev.off()
#'
#' isSimilar(file = "test4.png", fingerprint = fing1)
#' isSimilar(file = "test4.png", fingerprint = fing1, threshold = 5)
#'
#' png("test5.png")
#' hist(rdata2)
#' dev.off()
#'
#' isSimilar(file = "test5.png", fingerprint = fing1)
#' isSimilar(file = "test5.png", fingerprint = fing1, threshold = 5e6)
#' unlink("test1.png", "test2.png", "test3.png", "test4.png", "test5.png")

isSimilar <- function(file, fingerprint, threshold = 1e-3, exact = FALSE, ...) {
  
  if (missing(file)) { stop("file is missing") }
  if (missing(fingerprint)) { stop("fingerprint is missing") }
  if (!is.character(file) || length(file) != 1) { stop("file should be a single character") }
  
  if (is.numeric(fingerprint) || (is.character(fingerprint) || length(fingerprint) == 1)) {
    if (is.character(fingerprint) || length(fingerprint) == 1) {
      fingerprint <- getFingerprint(file = fingerprint, ...)
    }
    
  } else {
    stop("fingerprint should be a numeric vector or a single character")
  }
  
  test <- getFingerprint(file = file, ...)
  
  compareWithFingerprint(test = test, fingerprint = fingerprint, threshold = threshold, exact = exact)
}


#' @title compare a test fingerprint with a known fingerprint
#' @param test numeric vector
#' @param fingerprint numeric vector
#' @param threshold single numeric similarity parameter (default 1e-3)
#' @param exact single logical should fingerprints match exactly (default \code{FALSE})
#' @return single logical
#' @export
#' @examples
#'     compareWithFingerprint(test = 1:3, fingerprint = 1:3)
#'     compareWithFingerprint(test = 1:3, fingerprint = 1:4)
#'     compareWithFingerprint(test = 1:3, fingerprint = 1:3 + 1e-3)
#'     compareWithFingerprint(test = 1:3, fingerprint = c(1, 2, 3.1))
#'     compareWithFingerprint(test = 1:3, fingerprint = c(1, 2, 3.1), exact = TRUE)

compareWithFingerprint <- function(test, fingerprint, threshold = 1e-3, exact = FALSE) {
  
  result <- FALSE

  if (exact) {
    if (length(test) == length(fingerprint)) {
      if (all(test == fingerprint)) { 
        result <- TRUE
      }
    }

  } else {
    if (all(abs(fivenum(test) - fivenum(fingerprint)) <= abs(threshold))) {
      result <- TRUE
    }
  }

  result
}

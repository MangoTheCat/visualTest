
#' Plot some fingerprints against each other
#' This currently only works for fingerprints with the
#' \sQuote{original} algorithm.
#'
#' @param ... files to test
#' @param algorithm The algorithm to use, see
#'   \code{\link{getFingerprint}}. Currently only the \sQuote{original}
#'   algorithm works.
#' @return list of fingerprint(s) invisibly.
#'
#' @export
#' @importFrom methods allNames
#' @importFrom graphics plot lines
#'
#' @examples
#' sf <- system.file(package = "visualTest")
#' eg <- "VR-616_plot-lm00.jpg.gz"
#' showFingerprint(
#'   file.path(sf, "compare", "windows", eg),
#'   file.path(sf, "compare", "unix", eg)
#' )

showFingerprint <- function(..., algorithm = "original") {

  algorithm <- match.arg(algorithm)

  files <- list(...)
  if (length(files) == 0) stop("No files specified")

  fingers <- lapply(files, getFingerprint, algorithm = algorithm)

  rng <- range(unlist(fingers), na.rm = TRUE)
  lenf <- vapply(fingers, length, 1L)

  plot(c(1, max(lenf)), rng, type = "n", xlab = "", ylab = "")
  for (ff in seq_along(fingers)) lines(fingers[[ff]], col = ff, lty = ff)

  invisible(fingers)
}

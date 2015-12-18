
#' Plot some fingerprints against each other
#'
#' @param ... files to test
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

showFingerprint <- function(...) {

  files <- list(...)
  if (length(files) == 0) stop("No files specified")

  fingers <- lapply(files, getFingerprint)

  rng <- range(unlist(fingers), na.rm = TRUE)
  lenf <- vapply(fingers, length, 1L)

  plot(c(1, max(lenf)), rng, type = "n", xlab = "", ylab = "")
  for (ff in seq_along(fingers)) lines(fingers[[ff]], col = ff, lty = ff)

  invisible(fingers)
}

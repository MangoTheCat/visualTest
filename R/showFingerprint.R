
#' Show Fingerprint(s)
#' @param ... files to test
#' @return list of fingerprint(s) invisibly.
#'    As a side effect, a plot of fingerprints is created.
#' @export
#'
#' @examples
#' sf <- system.file(package = "visualTest")
#' eg <- "VR-616_plot-lm00.jpg"
#' showFingerprint(
#'   file.path(sf, "compare", "windows", eg),
#'   file.path(sf, "compare", "unix", eg)
#' )

showFingerprint <- function(...) {

  mcall <- match.call()
  lcall <- as.list(mcall)

  nf <- sum(allNames(lcall) == "") - 1
  fingers <- vector(mode = "list", length = nf)

  if (nf > 0) {
    lenf <-  vector(mode = "numeric", length = nf)
    for (ff in seq_along(fingers)) {
      fingers[[ff]] <- try(getFingerprint(eval(mcall[[ff + 1]])))
      lenf[ff] <- length(fingers[[ff]])
    }

    rng <- range(sapply(fingers, range, na.rm = TRUE), na.rm = TRUE)
    if (is.numeric(rng) && all(!is.na(rng)) && length(rng) == 2) {
      plot(c(1, max(lenf)), rng, type = "n", xlab = "", ylab = "")
      for (ff in seq_along(fingers)) {
        if (!is.null(fingers[[ff]])) {
          lines(seq_along(fingers[[ff]]), fingers[[ff]], col = ff, lty = ff) }
      }
    }
  }
  invisible(fingers)
}

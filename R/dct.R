
dct <- function(x) {
  size <- length(x)
  fact <- (seq_along(x) - 1/2) * pi / size

  vapply(seq_along(x) - 1, FUN.VALUE = 1, function(i) {
    sum(x * cos(i * fact)) * sqrt(2 / size)
  })
}

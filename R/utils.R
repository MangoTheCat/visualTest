
`%||%` <- function(l, r) if (is.null(l)) r else l

ungzip <- function(input, output = sub("\\.gz$", "", input)) {
  inp <- gzfile(input, open = "rb")
  data <- readBin(inp, what = "raw", n = 10^6)
  close(inp)
  writeBin(data, output)
}

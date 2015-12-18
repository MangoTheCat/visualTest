
make_plot <- function(ext) {

  iext <- sub("\\.gz$", "", ext)
  dev <- get(iext, envir = as.environment("package:grDevices"))

  f <- ff <- tempfile(fileext = paste0(".", iext))

  dev(f)
  plot(iris)
  dev.off()

  if (iext != ext) {
    ff <- paste0(f, ".gz")
    gzip(f, ff)
  }

  ff
}

gzip <- function(input, output = paste0(input, ".gz")) {
  data <- readBin(input, what = "raw", n = file.info(input)$size)
  out <- gzcon(file(output, open = "wb"))
  writeBin(data, out)
  close(out)
}


#' Get Image Fingerprint
#'
#' @param file single character naming PNG or JPG file
#'   from which to get fingerprint
#' @param ... additional arguments
#' @export
#' @examples
#' getFingerprint(
#'   file.path(system.file(package = "visualTest"), "compare", "stest-00.jpg")
#' )

getFingerprint <- function(file, ...) {

  if (missing(file) || length(file) == 0) { stop("file is missing") }
  if (length(file) > 1) { warning("only first value of file will be used") }

  file <- file[1]
  type <- "unknown"

  if (grepl(pattern = "*\\.[Pp][Nn][Gg]$", x = file)) { type <- "png" }
  if (grepl(pattern = "*\\.[Jj][Pp]([EeGg]|[Gg])$", x = file)) { type <- "jpg" }

  if (grepl(pattern = "*\\.[Bb][Mm][Pp]$", x = file)) {
    readBMP <- function(source) {
      img <- read.bmp(f = source)
      pow <- floor(max(img)^0.5)
      img <- img / 2^pow
      dm <- dim(img)
      if (length(dm) == 2) {
        img <- array(data = rep(c(img), times = 3), dim = c(nrow(img), ncol(img), 3))

      } else { if (length(dm) != 3) { stop("unexpected dimensions of source file") } }
      return(img)
    }
    type <- "bmp"
  }

  read <- switch(type, "png" = readPNG, "jpg" = readJPEG, "bmp" = readBMP, stop("unsupported file type"))
  imageArray <- read(file)
  imageMat <- rgb2Value(imageArray)

                                        # perform fast fourier transform
  ftImage <- mvfft(imageMat)

                                        # squash the signal into 1D
  sumImage <- apply(Im(ftImage), MARGIN = 1, sum)

  zeros <- isCross(x = sumImage, len = 3)
  diff(which(zeros))
}


#' Convert RGB array to value
#' @param array array with three dimensions length N, M and 3
#' @return matrix with nrow N and ncol M
#'
#' @keywords internal
#' @examples
#' rgba <- array(c(0:2, rep((1:8), times = 3)) / 10, dim = c(3, 3, 3))
#' rgb2Value(array = rgba)

rgb2Value <- function(array) {

  if (!is.array(array)) { stop("array must be an array") }
  dmArr <- dim(array)
  if (length(dmArr) != 3) { stop("array dimensions should be N, M, 3") }
  if (dmArr[3] != 3) { warning("unsupported number of channels, 3 expected") }

  N <- dmArr[1]
  M <- dmArr[2]

  rgbMat <- matrix(c(array[, , 1:3, drop = FALSE]), nrow = 3, ncol = N * M, byrow = TRUE)
  hsvMat <- rgb2hsv(r = rgbMat)
  val <- matrix(hsvMat[3, ], nrow = N, ncol = M, byrow = FALSE)

  return(val)
}


#' Find positions in a numeric vector where sign has changed on average for more than n elements
#'
#' @title Find Zero Crossing Points
#' @param x numeric vector
#' @param len natural number window length for sign comparison
#' @return logical vector of length zero
#' @examples
#'    x1 <- -7:8
#'    visualTest:::isCross(x = x1)
#'    x2 <- rep(-1:1, times = 6)
#'    visualTest:::isCross(x = x2)

isCross <- function(x, len = 3) {

  if (len < 1) { stop("len must be a natural number greater than 0") }

  matched <- zeros <- rep(FALSE, times = length(x))

  if (length(x) > len) {
    dx <- abs(c(rep(0, times = len), diff(sign(x), lag = len)))
    zeros <- dx == max(dx, na.rm = TRUE)

    if (all(zeros)) {
      zeros <- !zeros

    } else {
                                        # check for min length len change
      signature <- c(rep(TRUE, times = len - 1), FALSE)
      for (r in len + seq_len(length(zeros) - len)) {
        if (all(zeros[seq.int(from = r - len + 1, to = r)] == signature)) {
          matched[r - len + 1] <- TRUE
        }
      }
      zeros <- matched
    }

  } else {
    warning("x is shorter than len")
  }

  return(zeros)
}

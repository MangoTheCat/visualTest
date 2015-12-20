
`%||%` <- function(l, r) if (is.null(l)) r else l

ungzip <- function(input, output = sub("\\.gz$", "", input)) {
  inp <- gzfile(input, open = "rb")
  data <- readBin(inp, what = "raw", n = 10^6)
  close(inp)
  writeBin(data, output)
}

hexaTable <- c(
  "0000" = "0", "0001" = "1", "0010" = "2", "0011" = "3",
  "0100" = "4", "0101" = "5", "0110" = "6", "0111" = "7",
  "1000" = "8", "1001" = "9", "1010" = "A", "1011" = "B",
  "1100" = "C", "1101" = "D", "1110" = "E", "1111" = "F"
)

invHexaTable <- list(
  "0" = c(FALSE, FALSE, FALSE, FALSE),
  "1" = c(FALSE, FALSE, FALSE, TRUE),
  "2" = c(FALSE, FALSE, TRUE,  FALSE),
  "3" = c(FALSE, FALSE, TRUE,  TRUE),
  "4" = c(FALSE, TRUE,  FALSE, FALSE),
  "5" = c(FALSE, TRUE,  FALSE, TRUE),
  "6" = c(FALSE, TRUE,  TRUE,  FALSE),
  "7" = c(FALSE, TRUE,  TRUE,  TRUE),
  "8" = c(TRUE,  FALSE, FALSE, FALSE),
  "9" = c(TRUE,  FALSE, FALSE, TRUE),
  "A" = c(TRUE,  FALSE, TRUE,  FALSE),
  "B" = c(TRUE,  FALSE, TRUE,  TRUE),
  "C" = c(TRUE,  TRUE,  FALSE, FALSE),
  "D" = c(TRUE,  TRUE,  FALSE, TRUE),
  "E" = c(TRUE,  TRUE,  TRUE,  FALSE),
  "F" = c(TRUE,  TRUE,  TRUE,  TRUE)
)

logicalToHexa <- function(x) {
  stopifnot(is.logical(x))

  if (length(x) == 0) return(character())

  ## Need 4 bits for a hexa number, so length must be a
  ## multiple of four
  extra <- 4 - (length(x) %% 4)
  if (extra != 4) x <- c(logical(extra), x)

  ## To bit strings
  x <- apply(matrix(x + 0, nrow = 4), 2, paste, collapse = "")

  paste(hexaTable[x], collapse = "")
}

hexaToLogical <- function(x) {
  unname(unlist(invHexaTable[strsplit(x, "")[[1]]]))
}

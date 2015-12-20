
hamming_distance <- function(hex1, hex2) {
  sum(hexaToLogical(hex1) != hexaToLogical(hex2))
}

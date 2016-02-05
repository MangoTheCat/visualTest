
context("DCT fingerprinting")

test_that("Fingerprint of a base R plot is similar across platforms", {

  tmp <- tempfile(fileext = ".png")

  png(tmp)
  pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
        pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
  dev.off()

  print(getFingerprint(tmp))
  expect_true(isSimilar(tmp, "BA68E7B3948C8936"))

})

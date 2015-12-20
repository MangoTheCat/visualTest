
context("Image interpolation")

test_that("bilinearInterpolation", {

  rlogo <- system.file(package = "png", "img", "Rlogo.png")
  image <- png::readPNG(rlogo)

  small_image <- bilinearInterpolation(image, c(30, 40))

  expect_equal(dim(small_image), c(30, 40, 4))

})


context("Reading from compressed and not-compressed files")

test_that("not compressed files are OK", {

  expect_silent(getFingerprint(make_plot("png")))
  expect_silent(getFingerprint(make_plot("jpeg")))

})

test_that("compressed files are OK", {

  expect_silent(getFingerprint(make_plot("png.gz")))
  expect_silent(getFingerprint(make_plot("jpeg.gz")))

})

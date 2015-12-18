
context("show fingerprint")

test_that("showFingerprint", {

  sf <- system.file(package = "visualTest")
  eg <- "VR-616_plot-lm04.png.gz"

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp)
  showFingerprint(
    file.path(sf, "compare", "windows", eg),
    file.path(sf, "compare", "unix", eg)
  )
  dev.off()

  expect_equal(
    getFingerprint(tmp),
    c(9, 4, 4, 4, 10, 4, 7, 7, 6, 9, 7, 4, 9, 9, 7,
      4, 12, 3, 7, 10, 19, 10, 10, 19, 10, 16, 10, 3,
      10, 16, 10, 19, 10, 10, 19, 10, 7, 3, 12, 4, 7,
      9, 9, 4, 7, 9, 6, 7, 7, 4, 6, 8, 4, 4, 9, 4)
  )
})

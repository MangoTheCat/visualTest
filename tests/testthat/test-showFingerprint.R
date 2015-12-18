
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

  expect_true(
    isSimilar(tmp, "show-test.png.gz", threshold = 7)
  )
})

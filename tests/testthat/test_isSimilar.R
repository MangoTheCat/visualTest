
context("is this image similar?")

###############################################################################

test_that("isSimilar", {

  fingerprint <- c(26, 27, 26, 25, 4, 5, 22, 8, 3, 9, 6, 6, 3, 3,
                 3, 3, 3, 4, 8, 9, 3, 6, 24, 3, 6, 23, 26, 27, 24, 24)

  cases <- list(
    list(file = "stest-00.jpg.gz", correct = TRUE, threshold = 0.5),
    list(file = "stest-01.png.gz", correct = TRUE, threshold = 0),
    list(file = "stest-01.png.gz", correct = TRUE, exact = TRUE),
    list(file = "stest-02.png.gz", correct = FALSE),
    list(file = "stest-02.png.gz", correct = TRUE, threshold = 5),
    list(file = "stest-03.png.gz", correct = FALSE, exact = TRUE),
    list(file = "stest-03.png.gz", correct = TRUE),
    list(file = "stest-04.png.gz", correct = FALSE),
    list(file = "stest-04.png.gz", correct = TRUE, threshold = 4e3),
    list(file = "stest-05.png.gz", correct = FALSE),
    list(file = "stest-05.png.gz", correct = TRUE, threshold = 17),
    list(file = "stest-06.png.gz", correct = FALSE, threshold = 5),
    list(file = "stest-06.png.gz", correct = TRUE, threshold = 16)
  )

  for (case in cases) {
    case$file <- system.file(
      package = packageName(),
      "compare",
      case$file
    )

    corr <- case$correct
    case$correct <- NULL
    case$fingerprint <- fingerprint
    case$algorithm <- "original"

    result <- do.call(isSimilar, case)
    expect_equal(result, corr, info = paste(case$file, corr))
  }
})


test_that("compare with fingerprint", {
  expect_true (compareWithFingerprint(test = 1:3, fingerprint = 1:3))
  expect_false(compareWithFingerprint(test = 1:3, fingerprint = 1:4))
  expect_true (compareWithFingerprint(test = 1:3, fingerprint = 1:3 + 1e-3))
  expect_false(compareWithFingerprint(
    test = 1:3,
    fingerprint = c(1, 2, 3.1)
  ))
})


test_that("is cross-platform difference ignored?", {

  files <- list.files(
    system.file(package = packageName(), "compare", "unix")
  )

  thresholds <- c(
    "VR-616_plot-lm00.jpg.gz" = 3,
    "VR-616_plot-lm01.jpg.gz" = 9,
    "VR-616_plot-lm02.jpg.gz" = 3,
    "VR-616_plot-lm03.png.gz" = 10,
    "VR-616_plot-lm04.png.gz" = 11,
    "VR-616_plot-lm05.png.gz" = 3,
    "VR-616_plot-lm06.png.gz" = 4,
    "VR-616_plot-lm07.png.gz" = 4,
    "VR-616_plot-lm08.png.gz" = 3,
    "VR-616_plot-lm09.bmp.gz" = 3
  )

  for (f in files) {
    f1 <- system.file(package = packageName(), "compare", "unix", f)
    f2 <- system.file(package = packageName(), "compare", "windows", f)
    t <- thresholds[f]
    expect_true(
      isSimilar(
        file = f1,
        fingerprint = f2,
        threshold = t,
        algorithm = "original"
      ),
      info = f
    )
  }

})

test_that("isSimilar error handling", {

  expect_error(
    isSimilar(fingerprint = 1:10),
    "file is missing"
  )

  expect_error(
    isSimilar(file = "foobar"),
    "fingerprint is missing"
  )

  expect_error(
    isSimilar(file = 1:10, fingerprint = 1:10),
    "file should be a single character"
  )

  expect_error(
    isSimilar(file = c("a", "b"), fingerprint = 1:10),
    "file should be a single character"
  )

  expect_error(
    isSimilar(file = tempfile(), fingerprint = 1:10),
    "file does not exist"
  )

  tmp <- tempfile(fileext = ".png")
  cat("", file = tmp)
  on.exit(unlink(tmp), add = TRUE)

  expect_error(
    isSimilar(file = tmp, fingerprint = FALSE),
    "fingerprint should be a numeric vector or a single character"
  )

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  png(tmp)
  pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
        pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
  dev.off()

  expect_error(
    isSimilar(
      tmp,
      c(26, 27, 26, 25, 4, 5, 22, 8, 3, 9, 6, 6, 3, 3,
        3, 3, 3, 4, 8, 9, 3, 6, 24, 3, 6, 23, 26, 27, 24, 24)
    ),
    "Cannot compare fingerprints from different algorithms"
  )
})

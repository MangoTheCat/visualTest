
context("is this image similar?")

###############################################################################

test_that("isSimilar", {

  fingerprint <- c(26, 27, 26, 25, 4, 5, 22, 8, 3, 9, 6, 6, 3, 3,
                 3, 3, 3, 4, 8, 9, 3, 6, 24, 3, 6, 23, 26, 27, 24, 24)

  cases <- list(
    list(file = "stest-00.jpg", correct = TRUE, threshold = 0.5),
    list(file = "stest-01.png", correct = TRUE, threshold = 0),
    list(file = "stest-01.png", correct = TRUE, exact = TRUE),
    list(file = "stest-02.png", correct = FALSE),
    list(file = "stest-02.png", correct = TRUE, threshold = 5),
    list(file = "stest-03.png", correct = FALSE, exact = TRUE),
    list(file = "stest-03.png", correct = TRUE),
    list(file = "stest-04.png", correct = FALSE),
    list(file = "stest-04.png", correct = TRUE, threshold = 4e3),
    list(file = "stest-05.png", correct = FALSE),
    list(file = "stest-05.png", correct = TRUE, threshold = 17),
    list(file = "stest-06.png", correct = FALSE, threshold = 5),
    list(file = "stest-06.png", correct = TRUE, threshold = 16)
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
    "VR-616_plot-lm00.jpg" = 3,
    "VR-616_plot-lm01.jpg" = 9,
    "VR-616_plot-lm02.jpg" = 3,
    "VR-616_plot-lm03.png" = 10,
    "VR-616_plot-lm04.png" = 11,
    "VR-616_plot-lm05.png" = 3,
    "VR-616_plot-lm06.png" = 4,
    "VR-616_plot-lm07.png" = 4,
    "VR-616_plot-lm08.png" = 3,
    "VR-616_plot-lm09.bmp" = 3
  )

  for (f in files) {
    f1 <- system.file(package = packageName(), "compare", "unix", f)
    f2 <- system.file(package = packageName(), "compare", "windows", f)
    t <- thresholds[f]
    expect_true(
      isSimilar(file = f1, fingerprint = f2, threshold = t),
      info = f
    )
  }

})

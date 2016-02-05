
context("work out the image fingerprint")

###############################################################################

test_that("getFingerprint", {

  cases <- list(
    "stest-01.png.gz" = c(26, 27, 26, 25, 4, 5, 22, 8, 3, 9, 6, 6, 3, 3,
      3, 3, 3, 4, 8, 9, 3, 6, 24, 3, 6, 23, 26, 27, 24, 24),

    "stest-02.png.gz" = c(26, 27, 26, 28, 28, 8, 3, 9, 6, 9, 3, 3, 3, 6,
      9, 9, 3, 6, 28, 28, 26, 27, 26, 24),

    "stest-03.png.gz" = c(26, 27, 27, 24, 4, 5, 22, 8, 3, 9, 6, 6, 3, 3,
      3, 3, 3, 4, 8, 9, 3, 6, 24, 3, 6, 22, 27, 27, 26, 22),

    "stest-04.png.gz" = c(25, 28, 25, 28, 24, 28, 5, 3, 6, 3, 6, 3, 3, 26,
      26, 28, 25, 28, 25, 23),

    "stest-05.png.gz" = c(8, 6, 9, 7, 5, 8, 8, 5, 10, 7, 8, 5, 4, 3, 8,
      8, 10, 5, 5, 8, 7, 5, 8, 5, 5, 5, 8, 5, 7, 3,
      5, 7, 6, 5, 7, 5, 8, 5, 5, 5, 10, 5, 8, 10, 6,
      5, 4, 5, 8, 5, 10, 5, 8, 10, 5, 5, 8, 7, 6, 7),

    "stest-06.png.gz" = c(8, 7, 6, 8, 5, 13, 8, 3, 10, 5, 7, 6, 8, 5, 4,
      9, 8, 6, 12, 5, 4, 9, 8, 6, 7, 8, 5, 10, 8, 6,
      7, 8, 7, 6, 5, 8, 8, 10, 5, 8, 5, 8, 4, 9, 5,
      9, 4, 8, 8, 10, 8, 4, 9, 8, 4),

    "ftest-01.png.gz" = c(3, 7, 3, 9, 4, 3, 3, 23, 3, 3, 15, 3, 5, 21,
      3, 5, 14, 7, 6, 3, 5, 5, 3, 13, 6, 45, 7, 11, 3,
      5, 5, 6, 3, 7, 16, 3, 3, 23, 3, 3, 15, 3, 5, 21,
      3, 4, 3, 9, 3, 7),

    "ftest-02.png.gz" = c(5, 5, 4, 7, 8, 8, 16, 12, 9, 8, 5, 8, 8, 5, 3,
      3, 16, 9, 12, 8, 5, 8, 8, 5, 4, 7, 5, 8, 5, 8,
      8, 8, 11, 13, 8, 3, 5, 8, 4, 9, 8, 7, 12, 13, 13,
      8, 5, 6, 5, 5)
  )

  for (case in names(cases)) {
    file <- system.file(package = "visualTest", "compare", case)
    expect_equal(
      as.integer(cases[[case]]),
      unclass(getFingerprint(file = file, algorithm = "original")),
      info = case
    )
  }
})


context("convert RGB array to value matrix")

###############################################################################

test_that("rgb2Value", {

  rgba <- array(c(0:2, rep((1:8), times = 3)) / 10, dim = c(3, 3, 3))

  expect_equal(
    rgb2Value(array = rgba),
    structure(
      c(0.00313725490196078, 0.00313725490196078, 0.000784313725490196,
        0.00117647058823529, 0.00156862745098039, 0.00196078431372549,
        0.00235294117647059, 0.00274509803921569, 0.00313725490196078),
      .Dim = c(3, 3)
    )
  )

  rgba <- array(rep(0:1, times = 3), dim = c(1, 2, 3))

  expect_equal(
    rgb2Value(array = rgba),
    matrix(c(0, 0.00392156862745), nrow = 1, ncol = 2)
  )
})


context("find zero crossing points")

###############################################################################

test_that("isCross", {

  x1 <- -8:9
  a1 <- rep(FALSE, times = length(x1))

  expect_equal(isCross(x = x1, len = 10), a1)

  a1[10] <- TRUE
  expect_equal(isCross(x = x1), a1)
  expect_equal(isCross(x = x1, len = 5), a1)

  x2 <- rep(-1:1, times = 6)
  a2 <- rep(FALSE, times = length(x2))
  expect_equal(isCross(x = x2), a2)

  a3 <- rep(c(FALSE, TRUE, TRUE), times = length(x2) / 3)
  expect_equal(isCross(x = x2, len = 1), a3)

  x4 <- 8:-9
  expect_equal(isCross(x = x4), a1)

  x5 <- c(
    1.3262, 0.2851, -1.7754, -1.8482, 0.8696, 2.6825, 0.6367, 1.772,
    1.7225, 0.1437, 0.755, -0.0023, -0.8623, -1.3883, -0.9686, -0.3138,
    0.3704, -0.7423, 0.3733, 0.7867
  )
  a5 <- rep(FALSE, times = length(x5))
  a5[13] <- TRUE
  expect_equal(isCross(x = x5, len = 4), a5)
})

test_that("pkg_reader_error", {

  with_mock(
    `visualTest::pkg` = function(...) NULL,
    expect_error(
      pkg_reader_error("foobar")(),
      "package is needed"
    )
  )
})

test_that("bmp reader checks dimension", {

  expect_error(
    createBMP(function(...) 42)("foobar"),
    "unexpected dimensions"
  )

  expect_error(
    createBMP(function(...) array(42, dim = c(1,1,1,1)))("foobar"),
    "unexpected dimensions"
  )
})

test_that("isCross signals warning", {

  expect_warning(
    isCross(1:2, len = 3),
    "x is shorter than len"
  )
})

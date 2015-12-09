
context("work out the image fingerprint")

###############################################################################

test_that("getFingerprint", {
    
    finger00 <- c(20L, 26L, 27L, 27L, 24L, 4L, 5L, 22L, 8L, 3L, 9L, 6L, 6L, 3L, 
        3L, 3L, 3L, 3L, 4L, 8L, 9L, 3L, 6L, 24L, 3L, 6L, 22L, 27L, 27L, 24L, 23L, 4L)

    expect_that(getFingerprint(file = file.path(system.file(package = "visualTest"), "compare", "stest-00.jpg")), 
        equals(finger00))
    
    finger01 <- c(26L, 27L, 26L, 25L, 4L, 5L, 22L, 8L, 3L, 9L, 6L, 6L, 3L, 3L, 
        3L, 3L, 3L, 4L, 8L, 9L, 3L, 6L, 24L, 3L, 6L, 23L, 26L, 27L, 24L, 24L)
    
    expect_that(getFingerprint(file = file.path(system.file(package = "visualTest"), "compare", "stest-01.png")), 
        equals(finger01))
    
    finger02 <- c(26L, 27L, 26L, 28L, 28L, 8L, 3L, 9L, 6L, 9L, 3L, 3L, 3L, 6L, 
        9L, 9L, 3L, 6L, 28L, 28L, 26L, 27L, 26L, 24L)

    expect_that(getFingerprint(file = file.path(system.file(package = "visualTest"), "compare", "stest-02.png")), 
        equals(finger02))
    
    finger03 <- c(26L, 27L, 27L, 24L, 4L, 5L, 22L, 8L, 3L, 9L, 6L, 6L, 3L, 3L, 
        3L, 3L, 3L, 4L, 8L, 9L, 3L, 6L, 24L, 3L, 6L, 22L, 27L, 27L, 26L, 22L)
    
    expect_that(getFingerprint(file = file.path(system.file(package = "visualTest"), "compare", "stest-03.png")), 
        equals(finger03))
    
    finger04 <- c(25L, 28L, 25L, 28L, 24L, 28L, 5L, 3L, 6L, 3L, 6L, 3L, 3L, 26L, 
        26L, 28L, 25L, 28L, 25L, 23L)
    
    expect_that(getFingerprint(file = file.path(system.file(package = "visualTest"), "compare", "stest-04.png")), 
        equals(finger04))
    
    finger05 <- c(8L, 6L, 9L, 7L, 5L, 8L, 8L, 5L, 10L, 7L, 8L, 5L, 4L, 3L, 8L, 
        8L, 10L, 5L, 5L, 8L, 7L, 5L, 8L, 5L, 5L, 5L, 8L, 5L, 7L, 3L, 
        5L, 7L, 6L, 5L, 7L, 5L, 8L, 5L, 5L, 5L, 10L, 5L, 8L, 10L, 6L, 
        5L, 4L, 5L, 8L, 5L, 10L, 5L, 8L, 10L, 5L, 5L, 8L, 7L, 6L, 7L)

    expect_that(getFingerprint(file = file.path(system.file(package = "visualTest"), "compare", "stest-05.png")), 
        equals(finger05))
    
    finger06 <- c(8L, 7L, 6L, 8L, 5L, 13L, 8L, 3L, 10L, 5L, 7L, 6L, 8L, 5L, 4L, 
        9L, 8L, 6L, 12L, 5L, 4L, 9L, 8L, 6L, 7L, 8L, 5L, 10L, 8L, 6L, 
        7L, 8L, 7L, 6L, 5L, 8L, 8L, 10L, 5L, 8L, 5L, 8L, 4L, 9L, 5L, 
        9L, 4L, 8L, 8L, 10L, 8L, 4L, 9L, 8L, 4L)

    expect_that(getFingerprint(file = file.path(system.file(package = "visualTest"), "compare", "stest-06.png")), 
        equals(finger06))
    
#    png("visualTest/inst/compare/ftest-01.png", 
#        height = 400, width = 400, res = 72)
#    
#    plot(c(1, 100), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
#    
#    matlines(x = matrix(seq(from = 1, to = 100, length = 100), ncol = 2, nrow = 100), 
#        y = matrix(c(0, 1), nrow = 100, ncol = 2, byrow = FALSE), lwd = 1, col = 2, lty = 1)
#    
#    dev.off()
    
    fingerf01 <- c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
        4L, 4L, 4L, 4L, 4L, 4L, 4L, 12L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
        4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 12L, 
        4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
        4L, 4L, 4L, 4L, 4L, 4L, 12L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
        4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L)
    
    expect_that(getFingerprint(file = file.path(system.file(package = "visualTest"), "compare", "ftest-01.png")), 
        equals(fingerf01))
    
#    png("visualTest/inst/compare/ftest-02.png", 
#        height = 400, width = 400, res = 72)
#    
#    plot(c(1, 100), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
#    
#    matlines(x = matrix(seq(from = 1, to = 10, length = 100)^2, ncol = 2, nrow = 100), 
#        y = matrix(c(0, 1), nrow = 100, ncol = 2, byrow = FALSE), lwd = 1, col = 2, lty = 1)
#    
#    dev.off()
    
    expect_that(getFingerprint(file = file.path(system.file(package = "visualTest"), "compare", "ftest-02.png")), 
        equals(fingerf01))
    
    }
)


context("convert RGB array to value matrix")

###############################################################################

test_that("rgb2Value", {
    
    rgba <- array(c(0:2, rep((1:8), times = 3)) / 10, dim = c(3, 3, 3))
    
    expect_that(rgb2Value(array = rgba), equals(structure(c(0.00313725490196078, 0.00313725490196078, 
        0.000784313725490196, 0.00117647058823529, 0.00156862745098039, 0.00196078431372549, 
        0.00235294117647059, 0.00274509803921569, 0.00313725490196078), .Dim = c(3L, 3L))))
    
    rgba <- array(rep(0:1, times = 3), dim = c(1, 2, 3))
    
    expect_that(rgb2Value(array = rgba), equals(matrix(c(0, 0.00392156862745), nrow = 1, ncol = 2)))
    
})


context("find zero crossing points")

###############################################################################

test_that("isCross", {
    
    x1 <- -8:9
    
    a1 <- rep(FALSE, times = length(x1))
    
    expect_that(visualTest:::isCross(x = x1, len = 10), equals(a1))
    
    a1[10] <- TRUE
    
    expect_that(visualTest:::isCross(x = x1), equals(a1))
    
    expect_that(visualTest:::isCross(x = x1, len = 5), equals(a1))
    
    
    x2 <- rep(-1:1, times = 6)
    
    a2 <- rep(FALSE, times = length(x2))
    
    expect_that(visualTest:::isCross(x = x2), equals(a2))
    
    a3 <- rep(c(FALSE, TRUE, TRUE), times = length(x2) / 3)
    
    expect_that(visualTest:::isCross(x = x2, len = 1), equals(a3))
    
    x4 <- 8:-9
    
    expect_that(visualTest:::isCross(x = x4), equals(a1))
    
    x5 <- c(1.3262, 0.2851, -1.7754, -1.8482, 0.8696, 2.6825, 0.6367, 1.772, 
        1.7225, 0.1437, 0.755, -0.0023, -0.8623, -1.3883, -0.9686, -0.3138, 
        0.3704, -0.7423, 0.3733, 0.7867)
    
    a5 <- rep(FALSE, times = length(x5))
    
    a5[13] <- TRUE
    
    expect_that(visualTest:::isCross(x = x5, len = 4), equals(a5))
    
    rm(list = c("x1", "a1", "x2", "a2", "a3", "x4", "x5", "a5"))
})


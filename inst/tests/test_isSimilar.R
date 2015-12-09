
context("is this image similar?")

###############################################################################

test_that("isSimilar", {
    
    # file.path(system.file(package = "visualTest"), "compare", "stest-01.png")
    finger01 <- c(26L, 27L, 26L, 25L, 4L, 5L, 22L, 8L, 3L, 9L, 6L, 6L, 3L, 3L, 
        3L, 3L, 3L, 4L, 8L, 9L, 3L, 6L, 24L, 3L, 6L, 23L, 26L, 27L, 24L, 
        24L)
    
#    jpeg("visualTest/inst/compare/stest-06.jpg", 
#        height = 400, width = 400, res = 72)
#    
#    plot(rdata)
#    
#    dev.off()
    
    ### test 00 - file type change
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-00.jpg"), 
        fingerprint = finger01, threshold = 0.5), is_true())
        
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-00.jpg"), 
        fingerprint = finger01), is_false())
    
#    set.seed(4354353)
#    
#    rdata <- matrix(rnorm(200), ncol = 2)
#    
#    png("visualTest/inst/compare/stest-01.png", 
#        height = 400, width = 400, res = 72)
#    
#    plot(rdata)
#    
#    dev.off()
    
    ### test 01 - identity
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-01.png"), 
        fingerprint = finger01, threshold = 0), is_true())

    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-01.png"), 
        fingerprint = finger01, exact = TRUE), is_true())
    
#    png("visualTest/inst/compare/stest-02.png", 
#        height = 400, width = 400, res = 72)
#    
#    plot(rdata, col = 3)
#    
#    dev.off()
    
    ### test 02 - colour change
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-02.png"), 
        fingerprint = finger01), is_false())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-02.png"), 
        fingerprint = finger01, threshold = 5), is_true())

#    rdata2 <- rdata
#    
#    rdata2[2, 2] <- rdata2[2, 2] - 0.1
#    
#    png("visualTest/inst/compare/stest-03.png", 
#        height = 400, width = 400, res = 72)
#    
#    plot(rdata2)
#    
#    dev.off()
    
    ### test 03 - data change
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-03.png"), 
        fingerprint = finger01, exact = TRUE), is_false())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-03.png"), 
        fingerprint = finger01), is_true())
    
#    set.seed(987007)
#    
#    rdata3 <- matrix(rnorm(200), ncol = 2)
#    
#    png("visualTest/inst/compare/stest-04.png", 
#        height = 400, width = 400, res = 72)
#    
#    plot(rdata3)
#    
#    dev.off()
    
    ### test 04 - data change
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-04.png"), 
        fingerprint = finger01), is_false())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-04.png"), 
        fingerprint = finger01, threshold = 4e3), is_true())
    
#    png("visualTest/inst/compare/stest-05.png", 
#        height = 400, width = 400, res = 72)
#        
#    hist(rdata)
#    
#    dev.off()
    
    ### test 05 - type change
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-05.png"), 
        fingerprint = finger01), is_false())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-05.png"), 
        fingerprint = finger01, threshold = 17), is_true())
    
#    png("visualTest/inst/compare/stest-06.png", 
#        height = 400, width = 400, res = 84)
#    
#    plot(rdata)
#    
#    dev.off()
    
    ### test 06 - change resolution
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-06.png"), 
        fingerprint = finger01, threshold = 5), is_false())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "stest-06.png"), 
        fingerprint = finger01, threshold = 16), is_true())
    
    }
)


test_that("compare with fingerprint", {
    
    expect_that(compareWithFingerprint(test = 1:3, fingerprint = 1:3), is_true())
    
    expect_that(compareWithFingerprint(test = 1:3, fingerprint = 1:4), is_false())
    
    expect_that(compareWithFingerprint(test = 1:3, fingerprint = 1:3 + 1e-3), is_true())
    
    expect_that(compareWithFingerprint(test = 1:3, fingerprint = c(1, 2, 3.1)), is_false())
})


test_that("is cross-platform difference ignored?", {
    
    # see VALIDR \Tests\R 3.0.2\Base\testthat\test.stats.vr616.R
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm00.jpg"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm00.jpg"), 
        threshold = 3), is_true())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm01.jpg"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm01.jpg"), 
        threshold = 9), is_true())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm02.jpg"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm02.jpg"), 
        threshold = 3), is_true())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm03.png"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm03.png"), 
        threshold = 10), is_true())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm04.png"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm04.png"), 
        threshold = 11), is_true())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm05.png"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm05.png"), 
        threshold = 3), is_true())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm06.png"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm06.png"), 
        threshold = 4), is_true())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm07.png"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm07.png"), 
        threshold = 3), is_true())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm08.png"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm08.png"), 
        threshold = 3), is_true())
    
    expect_that(isSimilar(file = file.path(system.file(package = "visualTest"), "compare", "unix", "VR-616_plot-lm09.bmp"), 
        fingerprint = file.path(system.file(package = "visualTest"), "compare", "windows", "VR-616_plot-lm09.bmp"), 
        threshold = 3), is_true())
    }
)



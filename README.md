visualTest
==========

An [R package](http://www.r-project.org/) to perform fuzzy comparison of graphical files.
The threshold argument allows the level of fuzziness to be compared.

how to use this code
--------

To install this package you will need:
* R
* Rtools

Note that you must manually update your environmental variable PATH to include these utilities.

This package includes comments in **roxygen2** format. 
From R (and when located one level above folder visualTest) run the command 
`roxygenise("visualTest")` to build the help files. 
To build the package on Windows use the command `R CMD INSTALL --build visualTest` from the command window.
This package also includes tests in **testthat** format. From R run the call `test_package("visualTest")`.
   

```R
set.seed(4354353)
rdata <- matrix(rnorm(200), ncol = 2)
png("stest-01.png", height = 400, width = 400, res = 72)
plot(rdata)
dev.off()
finger01 <- getFingerprint(file = "stest-01.png")
finger01
#  [1] 26 27 26 25  4  5 22  8  3  9  6  6  3  3  3  3
# [17]  3  4  8  9  3  6 24  3  6 23 26 27 24 24
```

![random normal data](https://raw.githubusercontent.com/MangoTheCat/visualTest/master/inst/compare/stest-01.png "plot(rdata)")

```R
png("stest-02.png", height = 400, width = 400, res = 72)
plot(rdata, col = 3)
dev.off()
isSimilar(file = "test2.png", fingerprint = finger01)
# [1] FALSE
isSimilar(file = "test2.png", fingerprint = finger01, threshold = 12)
# [1] TRUE
```

![random normal data green](https://raw.githubusercontent.com/MangoTheCat/visualTest/master/inst/compare/stest-02.png "plot(rdata, col = 3)")

```R
png("stest-05.png", height = 400, width = 400, res = 72)
hist(rdata)
dev.off()
isSimilar(file = "stest-05.png", fingerprint = finger01)
# [1] FALSE
isSimilar(file = "stest-05.png", fingerprint = finger01, threshold = 17)
# [1] TRUE
```

![random normal data histogram](https://raw.githubusercontent.com/MangoTheCat/visualTest/master/inst/compare/stest-05.png "hist(rdata)")
 

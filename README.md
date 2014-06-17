visualTest
==========

An [R package](http://www.r-project.org/) to perform fuzzy testing of graphical files.

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
```

![random normal data](https://raw.githubusercontent.com/MangoTheCat/visualTest/master/inst/compare/stest-01.png "plot(rdata)")

```R
png("stest-02.png", height = 400, width = 400, res = 72)
plot(rdata, col = 3)
dev.off()
isSimilar(file = "test2.png", fingerprint = finger01)
isSimilar(file = "test2.png", fingerprint = finger01, threshold = 5)
```

![random normal data green](https://raw.githubusercontent.com/MangoTheCat/visualTest/master/inst/compare/stest-02.png "plot(rdata, col = 3)")

```R
png("stest-05.png", height = 400, width = 400, res = 72)
hist(rdata)
dev.off()
isSimilar(file = "stest-05.png", fingerprint = finger01)
isSimilar(file = "stest-05.png", fingerprint = finger01, threshold = 17)
```

![random normal data histogram](https://raw.githubusercontent.com/MangoTheCat/visualTest/master/inst/compare/stest-05.png "hist(rdata)")
 

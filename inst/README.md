
# visualTest

[![Linux Build Status](https://travis-ci.org/MangoTheCat/visualTest.svg?branch=master)](https://travis-ci.org/MangoTheCat/visualTest)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/MangoTheCat/visualTest?svg=true)](https://ci.appveyor.com/project/gaborcsardi/visualTest)
[![](http://www.r-pkg.org/badges/version/visualTest)](http://www.r-pkg.org/pkg/visualTest)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/visualTest)](http://www.r-pkg.org/pkg/visualTest)
[![Coverage Status](https://img.shields.io/codecov/c/github/MangoTheCat/visualTest/master.svg)](https://codecov.io/github/MangoTheCat/visualTest?branch=master)

An [R package](http://www.r-project.org/) to perform fuzzy comparison
of graphical files. The threshold argument allows the level of fuzziness
to be compared.

## Usage


```R
# install devtools for devtools::install_github
install.packages("devtools")
require(devtools)
# install visualTest
install_github("MangoTheCat/visualTest")
```

```R
require(visualTest)
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
 
## License

GPL 2 © [Mango Solutions](https://github.com/mangothecat).

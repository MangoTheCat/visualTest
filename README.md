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
   

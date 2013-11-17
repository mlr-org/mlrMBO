mlrMBO
======

Model-based optimization with mlr 

Offical CRAN release site: 
http://cran.r-project.org/web/packages/mlrMBO/index.html

R Documentation in HTML:
http://www.statistik.tu-dortmund.de/~bischl/rdocs/mlrMBO/

Travis CI: [![Build Status](https://travis-ci.org/berndbischl/mlrMBO.png)](https://travis-ci.org/berndbischl/mlrMBO)

Both, the CRAN release site and the R Documentation, will be released soon.


Installation
============

1) Normal users:
Please use the CRAN version linked above.

2) Early adopters: Simply running
```r
devtools::install_github("mlrMBO", username="berndbischl")
```
will install the current github version.

3) Developers and hackers:

You can install a new package version after local code changes if you are in the checkout directory via
```r
devtools::install(".")
```
Assuming you have a reasonably configured OS and R, you could also build and run tasks via the MAKEFILE.
But only a VERY SMALL percentage of users should be interested in this.

- Clone from git repo here

- Have recent version of R properly installed with all build tools. For Windows this will include 
  
  http://cran.r-project.org/bin/windows/Rtools/

- Have R, Rscript and the binaries of Rtools in your PATH 

- Have roxygen2, devtools and testhat R packages installed

- In a console run "make install" to install. Done.

- "make" will list all other build targets


Mailinglist and Email-Service-Hook
==================================

Developers use this mailing list:

https://groups.google.com/forum/?hl=de#!forum/mlrmbo-devel

mlrmbo-devel@googlegroups.com

The github-service-hook will also send commit messages to this list. 









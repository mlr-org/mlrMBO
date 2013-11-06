library(methods)
library(testthat)
library(devtools)
library(mlr)
library(soobench)

if (interactive()) {
  load_all(".", reset=TRUE)
} else {
  library(mlrMBO)  
}

configureMlr(show.learner.output=FALSE)
test_dir("inst/tests/")

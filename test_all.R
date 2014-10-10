library(methods)
library(checkmate)
library(testthat)
library(devtools)
library(mlr)
library(soobench)

load_all(".", reset = TRUE)

configureMlr(show.learner.output = FALSE)
test_dir("tests/testthat")


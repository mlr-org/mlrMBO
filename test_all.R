library(methods)
library(checkmate)
library(testthat)
library(devtools)
library(mlr)
library(soobench)

load_all(".", reset = TRUE)

configureMlr(show.learner.output = FALSE)
options(mlrMBO.show.info = FALSE)
test_dir("tests/testthat")


library(testthat)
library(mlrMBO)
library(checkmate)

seed.val = sample(1:100)
set.seed(seed.val)
catf("Run test with seed: %i", seed.val)
test_check("mlrMBO")

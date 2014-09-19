context("nsga2_vectorized")

test_that("nsga2_vectorized", {
  
  fun = function(x) {
    apply(x, 1, function(x) x^2)
  }
  res = nsga2_vectorized(fun, idim= 5, odim = 2, lower.bounds = rep(0, 5),
    upper.bounds = rep(1, 5), popsize = 12L, generations = 12L)
  # check if we reached the optimum. yes, this is kind of stochastic, but the
  # tests are very cautious
  expect_true(all(apply(res$value, 2, min) < 0.1))
  
  fun2 = function(x) {
    apply(x, 1, zdt1)
  }
  res = nsga2_vectorized(fun, idim= 5, odim = 2, lower.bounds = rep(0, 5),
    upper.bounds = rep(1, 5), popsize = 12L, generations = 12L)
  dom.hv = getDominatedHV(res$value, c(11, 11), minimize = c(TRUE, TRUE))
  expect_true(dom.hv > 100)
  expect_true(all(dim(res$par) == c(12, 5)))
  expect_true(all(dim(res$value) == c(12, 2)))
})

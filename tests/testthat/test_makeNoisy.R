context("make noisy")

test_that("make noisy function out of 1D smoof function", {
  ps = makeNumericParamSet("x", lower = -3, upper = 3)
  f = smoof::makeSingleObjectiveFunction(
    fn = function(x) x^2,
    par.set = ps
  )
  noise = function(x) 2
  fnoisy = makeNoisy(f, noise)

  expect_true(isNoisy(fnoisy))

  teval = replicate(10, f(1))
  neval = replicate(10, fnoisy(1))
  expect_true(all(teval[1] == teval))
  expect_true(!all(neval[1] == neval))
})

test_that("make noisy function out of 5D smoof function", {
  f = smoof::makeAckleyFunction(5L)
  noise = function(x) 2
  fnoisy = makeNoisy(f, noise)

  expect_true(isNoisy(fnoisy))

  teval = replicate(10, f(1:5))
  neval = replicate(10, fnoisy(1:5))
  expect_true(all(teval[1] == teval))
  expect_true(!all(neval[1] == neval))
})

test_that("keep meta-information when making noisy", {
  f = makeSphereFunction(1)
  noise = function(x) 2
  fnoisy = makeNoisy(f, noise)

  expect_true(isNoisy(fnoisy))
  expect_equal(getGlobalOptimum(fnoisy), getGlobalOptimum(f))
  expect_equal(getLocalOptimum(fnoisy), getLocalOptimum(f))

  mf = smoof::getMeanFunction(fnoisy)
  evalmf = vapply(1:10, mf, FUN.VALUE = numeric(1))
  evalf = vapply(1:10, f, FUN.VALUE = numeric(1))
  expect_equal(evalmf, evalf)
})

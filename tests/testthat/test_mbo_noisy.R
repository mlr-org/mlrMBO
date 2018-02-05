context("mbo noisy")

test_that("mbo works with multiple instances of noisy problems", {
  ps = makeNumericParamSet("x", 1, -7, 7)
  fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) x^2 + rnorm(1, 0, 0.5),
    par.set = ps,
    noisy = TRUE
  )
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.aei, opt.focussearch.points = 100L)
  ctrl = setMBOControlNoisy(ctrl, instances = 5L)
  or = mbo(fun, control = ctrl)
  opdf = as.data.frame(or$opt.path)
  expect_true(all(table(opdf$x) == 5))
})

test_that("mbo works with multiple fixed instances of noisy problems", {
  ps = makeNumericParamSet("x", 1, -7, 7)
  fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) x$x^2 + rnorm(1, (x$i - 2)/100, 0.05),
    par.set = ps,
    noisy = TRUE,
    has.simple.signature = FALSE
  )
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.aei, opt.focussearch.points = 100L)
  ctrl = setMBOControlNoisy(ctrl, instances = 5L, instance.param = "i")
  or = mbo(fun, control = ctrl)
  opdf = as.data.frame(or$opt.path)
  expect_true(all(opdf$i %in% 1:5))
  expect_true(all(table(opdf$x) == 5))
})

test_that("mbo works with self replicating instances of noisy problems", {
  ps = makeNumericParamSet("x", 1, -7, 7)
  fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) x^2 + rnorm(5, 0.01),
    par.set = ps,
    noisy = TRUE
  )
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.aei, opt.focussearch.points = 100L)
  ctrl = setMBOControlNoisy(ctrl, instances = 5L, self.replicating = TRUE)
  or = mbo(fun, control = ctrl)
  opdf = as.data.frame(or$opt.path)
  expect_true(all(opdf$noisy.repl %in% 1:5))
  expect_true(all(table(opdf$x) == 5))
})

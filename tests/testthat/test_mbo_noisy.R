context("mbo noisy")

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
  ctrl = setMBOControlNoisy(ctrl, self.replicating = TRUE)
  or = mbo(fun, control = ctrl)
  opdf = as.data.frame(or$opt.path)
  expect_true(all(opdf$noisy.repl %in% 1:5))
  expect_true(all(table(opdf$x) == 5))

  # now the function has varying n results
  i = 0
  fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      i <<- i + 1
      x^2 + rnorm(i, 0.01)
    },
    par.set = ps,
    noisy = TRUE
  )
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.aei, opt.focussearch.points = 100L)
  ctrl = setMBOControlNoisy(ctrl, self.replicating = TRUE)
  or = mbo(fun, control = ctrl)
  opdf = as.data.frame(or$opt.path)
  expect_true(all(opdf$noisy.repl == unlist(lapply(1:6, function(x) head(1:6,x)))))

  # returns the right result
  ps = makeNumericParamSet("x", 2, -7, 7)
  fun = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      x = sum(unlist(x))
      res = x^2 + rnorm(5, 0.01)
      if (abs(x)>5) res[1] = -10
      return(res)
    },
    par.set = ps,
    noisy = TRUE
  )
  ctrl = makeMBOControl(final.method = "best.predicted")
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.aei, opt.focussearch.points = 100L)
  ctrl = setMBOControlNoisy(ctrl, self.replicating = TRUE)
  or = mbo(fun, control = ctrl)
  expect_true(abs(sum(or$x$x))<5)
})


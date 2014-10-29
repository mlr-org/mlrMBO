context("test user extras")

test_that("user can log extra stuff to optimization path via the 'extras' attribute of the target function values", {

  extras.names = c("extra1", "extra2")

  obj.fn = function(x) {
    res = sum(x$x^2)
    # here we append additional stuff to be logged in opt path
    res = setAttribute(res, "extras", list(extra1 = runif(1), extra2 = letters[sample(1:10, 1)]))
    return(res)
  }

  par.set = makeParamSet(
    makeNumericParam("x", lower = -2, upper = 2)
    )

  learner = makeLearner("regr.km", predict.type = "se")
  control = makeMBOControl(init.design.points = 5L, iters = 2L)
  control = setMBOControlInfill(control, crit = "ei", opt = "focussearch")

  # run mbo and convert opt path to data frame
  result = mbo(obj.fn, par.set, learner = learner, control = control)
  opt.path = as.data.frame(result$opt.path)

  # check that user extras are in fact logged
  expect_true(all(extras.names %in% names(opt.path)))
  expect_true(all(is.numeric(opt.path[["extra1"]])))
  expect_true(all(opt.path[["extra2"]] %in% letters[1:10]))
})

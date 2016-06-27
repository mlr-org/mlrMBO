context("test optpath extras")

test_that("extras are logged a expected", {

  # Our test function stores additional stuff here via attributes
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      res = sum(x^2)
      # here we append additional stuff to be logged in opt path
      res = setAttribute(res, "extras", list(extra1 = runif(1), extra2 = letters[sample(1:10, 1)]))
      return(res)
    },
    par.set = makeParamSet(
      makeNumericParam("x", lower = -2, upper = 2)
    )
  )
  des = generateTestDesign(10L, getParamSet(f))

  learner = makeLearner("regr.km", predict.type = "se")
  control = makeMBOControl()
  control = setMBOControlTermination(control, iters = 2L)
  control = setMBOControlInfill(control, crit = "ei", opt = "focussearch")

  # run mbo and convert opt path to data frame
  result = mbo(f, des, learner = learner, control = control)

  extras.names = c("extra1", "extra2")

  opt.path = as.data.frame(result$opt.path)

  # check whether user extras are in fact logged
  expect_true(all(extras.names %in% names(opt.path)))
  expect_true(all(is.numeric(opt.path[["extra1"]])))
  expect_true(all(opt.path[["extra2"]] %in% letters[1:10]))

  # check whether times (model fitting, execution, ...) are logged
  expect_true(all(is.numeric(opt.path[["exec.time"]])))
  expect_true(all(is.numeric(opt.path[["train.time"]])))
  expect_true(all(is.numeric(opt.path[["propose.time"]])))
})

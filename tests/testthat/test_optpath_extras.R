context("test optpath extras")

test_that("extras are logged a expected", {
  # Our test function stores additional stuff here via attributes
  f = makeSingleObjectiveFunction(
    fn = function(x) {
      res = sum(x^2)
      # here we append additional stuff to be logged in opt path
      extras = list(extra1 = runif(1), extra2 = letters[sample(1:10, 1)])
      if (res > 2) extras = c(extras, list(.extra3 = "y bigger than two!"))
      res = setAttribute(res, "extras", extras)
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
  control = setMBOControlInfill(control, crit = crit.ei, opt = "focussearch")

  # run mbo and convert opt path to data frame
  result = mbo(f, des, learner = learner, control = control)
  extras.names = c("extra1", "extra2")

  opt.path = as.data.frame(result$opt.path)

  # check whether user extras are in fact logged
  expect_subset(extras.names, names(opt.path))
  expect_numeric(opt.path[["extra1"]])
  expect_subset(as.character(opt.path[["extra2"]]), letters[1:10])

  # check if the dotted extra is logged
  expect_equal(getOptPathEl(result$opt.path, which.first(opt.path$y > 2))$extra$.extra3, "y bigger than two!")

  # check whether times (model fitting, execution, ...) are logged
  expect_numeric(opt.path[["exec.time"]], any.missing = FALSE)
  expect_numeric(opt.path[["train.time"]])
  expect_numeric(opt.path[["propose.time"]])

  #check whether infill.crit specific informations is logged
  expect_subset(c("se", "mean"), names(opt.path))
  expect_numeric(opt.path[["se"]][11:12], any.missing = FALSE)
  expect_numeric(opt.path[["mean"]][11:12], any.missing = FALSE)
})

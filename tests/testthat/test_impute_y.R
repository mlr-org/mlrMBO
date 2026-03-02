context("impute y")

test_that("impute y", {

  # silent mode
  options(parallelMap.suppress.local.errors = TRUE)

  par.set = makeNumericParamSet(len = 2L, lower = 0, upper = 3)
  f1 = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      y = sum(x^2)
      if (y < 5)
        return(NA)
      return(y)
    },
    par.set = par.set
  )

  f2 = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      y = sum(x^2)
      if (y < 5)
        stop("foo")
      return(y)
    },
    par.set = par.set
  )

  learner = makeLearner("regr.randomForest", predict.type = "se", se.method = "sd")

  n.focus.points = 100L
  des1 = generateTestDesign(10L, getParamSet(f1))
  des2 = generateTestDesign(10L, getParamSet(f2))

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 20L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = n.focus.points)
  expect_error(mbo(f1, des1, learner, ctrl), "must be a numeric of length 1")

  ctrl = makeMBOControl(impute.y.fun = function(x, y, opt.path) 0)
  ctrl = setMBOControlTermination(ctrl, iters = 20L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = n.focus.points)
  res = mbo(f1, des2, learner, ctrl)
  expect_is(res, "MBOResult")

  # Check for correct error messages
  na.inds = which(getOptPathY(res$opt.path) == 0)
  for (ind in 1:getOptPathLength(res$opt.path)) {
    if (ind %in% na.inds)
      expect_string(getOptPathErrorMessages(res$opt.path)[ind], fixed = "mlrMBO:")
    else
      expect_equal(NA_character_, getOptPathErrorMessages(res$opt.path)[ind])
  }

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 50L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = n.focus.points)
  expect_error(mbo(f2, des2, learner, ctrl), "foo")
  ctrl = makeMBOControl(impute.y.fun = function(x, y, opt.path) 0)
  ctrl = setMBOControlTermination(ctrl, iters = 50L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = n.focus.points)
  res = mbo(f2, des2, learner, ctrl)
  expect_is(res, "MBOResult")

  # Check for correct error messages
  na.inds = which(getOptPathY(res$opt.path) == 0)
  for (ind in 1:getOptPathLength(res$opt.path)) {
    if (ind %in% na.inds)
      expect_string(getOptPathErrorMessages(res$opt.path)[ind], fixed = "foo")
    else
      expect_equal(NA_character_, getOptPathErrorMessages(res$opt.path)[ind])
  }

  # turn off silent mode
  options(parallelMap.suppress.local.errors = FALSE)
})

test_that("impute y parego", {
  # Test impute
  f1 = smoof::makeMultiObjectiveFunction(
    fn = function(x) {
      y = x^2
      ifelse(y < 2, NA, y)
    },
    par.set = makeNumericParamSet(len = 2L, lower = 0, upper = 3),
    n.objectives = 2L
  )
  des1 = generateTestDesign(10L, getParamSet(f1))
  learner = makeLearner("regr.rpart")
  ctrl = makeMBOControl(n.objectives = 2L,
    impute.y.fun = function(x, y, opt.path) c(100, 100))
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.mr, opt.focussearch.points = 10)
  ctrl = setMBOControlMultiObj(ctrl, method = "parego", parego.s = 100)
  or = mbo(f1, des1, learner = learner, control = ctrl)
  op = as.data.frame(or$opt.path)
  expect_data_frame(op, nrow = 10 + 5)
  expect_true(all(is.na(op$error.message) | op$y1 == 100))
})

test_that("imputation works with extras", {

  # silent mode
  options(parallelMap.suppress.local.errors = TRUE)

  par.set = makeNumericParamSet(len = 2L, lower = 0, upper = 3)

  f1 = smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      y = sum(x^2)
      if (y < 5)
        return(NA)
      if (y > 15)
        stop("simulated error")
      attr(y, "extras") = list(.dotextra = list("pass"), x1 = x[[1]], x2 = x[[2]])
      return(y)
    },
    par.set = par.set
  )

  learner = makeLearner("regr.randomForest", predict.type = "se", se.method = "sd")

  n.focus.points = 100L
  design1 = data.frame(x1 = rep(seq(2, 3, length.out = 5), 2), x2 = rep(c(2, 3), each = 5))
  design2 = data.frame(x1 = rep(seq(1, 2, length.out = 5), 2), x2 = rep(c(1, 2), each = 5))

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 20L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = n.focus.points)
  expect_error(mbo(f1, design1, learner, ctrl), "simulated error")
  expect_error(mbo(f1, design2, learner, ctrl), "must be a numeric of length 1")

  ctrl = makeMBOControl(impute.y.fun = function(x, y, opt.path) 0)
  ctrl = setMBOControlTermination(ctrl, iters = 20L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = n.focus.points)

  # valid points added first, then imputation is missing extras
  expect_error(mbo(f1, design1, learner, ctrl), "Trying to add extras but missing.*x1.*x2")

  # invalid points evaluated first, which are imputed w/o extras. Valid points then have unexpected extras
  expect_error(mbo(f1, design2, learner, ctrl), "Trying to add unknown extra.*x1.*x2")


  ctrl = makeMBOControl(impute.y.fun = function(x, y, opt.path) {
    structure(20, extras = list(.dotextra = list("fail"), x1 = -x$x[[1]], x2 = -x$x[[2]]))
  })

  ctrl = setMBOControlTermination(ctrl, iters = 20L)
  ctrl = setMBOControlInfill(ctrl, opt.focussearch.points = n.focus.points)

  res = mbo(f1, design1, learner, ctrl)
  expect_is(res, "MBOResult")

  # Check for correct error messages
  na.inds = which(getOptPathY(res$opt.path) == 20)
  opex = getOptPathX(res$opt.path)
  expected.error.above = which(rowSums(opex^2) > 15)
  expected.error.below = which(rowSums(opex^2) < 5)
  expect_set_equal(na.inds, c(expected.error.above, expected.error.below))
  for (ind in 1:getOptPathLength(res$opt.path)) {
    opel = getOptPathEl(res$opt.path, ind)
    if (ind %in% na.inds) {
      expect_equal(unname(opel$y), 20)
      expect_equal(opel$extra[c(".dotextra", "x1", "x2")], list(.dotextra = list("fail"), x1 = -opex[ind, 1], x2 = -opex[ind, 2]))
    } else {
      expect_equal(unname(opel$y), unname(sum(opex[ind, ]^2)))
      expect_equal(opel$extra[c(".dotextra", "x1", "x2")], list(.dotextra = list("pass"), x1 = opex[ind, 1], x2 = opex[ind, 2]))
    }
    if (ind %in% expected.error.above) {
      expect_string(getOptPathErrorMessages(res$opt.path)[ind], fixed = "simulated error")
    } else if (ind %in% expected.error.below) {
      expect_string(getOptPathErrorMessages(res$opt.path)[ind], fixed = "mlrMBO:")
    } else {
      expect_equal(NA_character_, getOptPathErrorMessages(res$opt.path)[ind])
    }
  }

  # turn off silent mode
  options(parallelMap.suppress.local.errors = FALSE)
})

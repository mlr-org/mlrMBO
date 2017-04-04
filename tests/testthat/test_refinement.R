context("refinement")

test_that("basic refinement works with rf and km", {

  par.set = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeIntegerParam("x2", lower = -1, upper = 2),
    makeDiscreteParam("x3", values = c("a", "b"))
  )

  f = makeSingleObjectiveFunction(
    fn = function(x) {
      if (x[[3]] == "a")
        x[[1]]^2+x[[2]]^2
      else
        x[[1]]^2+x[[2]]^2 + 2
    },
    par.set = par.set,
    has.simple.signature = FALSE
  )
  # f = smoof::addCountingWrapper(f)
  des = generateTestDesign(10, par.set = par.set)
  learner.ref = makeLearner("regr.km", predict.type = "se")
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlInfill(ctrl, crit = crit.cb, opt.restarts = 1, opt.focussearch.points = 100, opt.focussearch.maxit = 5)
  ctrl.ref = makeMBOControl()
  ctrl.ref = setMBOControlInfill(ctrl, crit = crit.ei)
  ctrl = setMBOControlRefinement(ctrl, refinement.control = ctrl.ref, refinement.learner = learner.ref)
  or = mbo(f, design = des, control = ctrl, show.info = FALSE)
  expect_number(or$y)
  expect_equal(or$y, f(or$x))
  expect_equal(getOptPathLength(or$opt.path), 15)
  expect_list(or$x)
  expect_true(or$y < 2)

  op = as.data.frame(or$opt.path)
  expect_true(all(op[11:15, "prop.type"] == "infill_cb_refined_infill_ei"))
  expect_subset(c("se", "mean", "lambda"), colnames(op))
})

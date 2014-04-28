context("init design")

test_that("mbo works with init design", {
  objfun = function(x) x^2

  ps = makeNumericParamSet(len = 1, lower = -5, upper = 5)

  design.x = generateDesign(5, ps)
  y = apply(design.x, 1, objfun)
  design = cbind(design.x, y)
  attr(design, "trafo") = FALSE
  learner_km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", nugget.estim = TRUE)

  ctrl = makeMBOControl(
    iters = 5,
    infill.crit = "ei",
    init.design.points = 10,
    infill.opt = "focussearch"
  )

  or = mbo(makeMBOFunction(objfun), design = design, par.set = ps, learner = learner_km, control = ctrl, show.info =TRUE)
  expect_true(!is.null(or))
})


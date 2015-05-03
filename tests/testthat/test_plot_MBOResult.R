context("plot MBO Result")

test_that("plot single crit", {
  f = makeMBOFunction(function(x) crossprod(x))
  ps = makeNumericParamSet(len = 5L, lower = -1, upper = 1)
  learner = makeLearner("regr.km", predict.type = "se")
  ctrl = makeMBOControl(iters = 2L, number.of.targets = 1L, init.design.points = 8L,
    propose.points = 1L)
  ctrl = setMBOControlInfill(ctrl, crit = "ei", opt.focussearch.points = 100L, 
    opt.focussearch.maxit = 3L)
  or = mbo(f, ps, learner = learner, control = ctrl)
  
  plots = renderMBOPlot(or, 0, extra.measures = "train.time")
  expect_equal(length(plots), 2)
  expect_error(renderMBOPlot(or, 0, hv.plot = TRUE))
  plots = renderMBOPlot(or, 1, extra.measures = "train.time")
  expect_equal(length(plots), 4)
  plots = renderMBOPlot(or, 1, extra.measures = "train.time",
    lim.x = list(YSpace = c(-1, 1), CritPlot = c(-1, 1), ExtraPlot1 = c(-1, 1), XSpace = c(-1, 1)),
    lim.y = list(YSpace = c(-1, 1), CritPlot = c(-1, 1), ExtraPlot1 = c(-1, 1), XSpace = c(-1, 1))
    )
  plots = renderMBOPlot(or, 2, extra.measures = "train.time", 
    colours = c("black", "orange", "yellow", "green"), 
    scale = "robust", size = c(2,2)
  )
  plotMBOResult(or, 0:2, extra.measures = "train.time", pause  = FALSE)
})

test_that("plot multi crit", {
  f = makeMBOFunction(zdt1)
  ps = makeNumericParamSet(len = 5L, lower = 0, upper = 1)
  learner = makeLearner("regr.km", predict.type = "se")
  ctrl = makeMBOControl(iters = 2L, number.of.targets = 2L, init.design.points = 8L,
    propose.points = 2L)
  ctrl = setMBOControlInfill(ctrl, crit = "dib", opt.focussearch.points = 100L, 
    opt.focussearch.maxit = 3L)
  ctrl = setMBOControlMultiCrit(ctrl, method = "dib", dib.indicator = "sms")
  or = mbo(f, ps, learner = learner, control = ctrl)
  
  expect_error(renderMBOPlot(or, 0, extra.measures = "train.time"))
  plots = renderMBOPlot(or, 0)
  expect_equal(length(plots), 3)
  plots = renderMBOPlot(or, 1)
  expect_equal(length(plots), 4)
  plots = renderMBOPlot(or, 1,
    lim.x = list(YSpace = c(-1, 1), CritPlot = c(-1, 1), ExtraPlot1 = c(-1, 1), XSpace = c(-1, 1)),
    lim.y = list(YSpace = c(-1, 1), CritPlot = c(-1, 1), ExtraPlot1 = c(-1, 1), XSpace = c(-1, 1)),
    ref.point = c(11, 11)
  )
  plots = renderMBOPlot(or, 2, colours = c("black", "orange", "yellow", "green"), 
    scale = "robust", size = c(2,2)
  )
  plotMBOResult(or, 0:2, pause  = FALSE)
})



addAlgorithm(reg, id = "random", fun = function(static, iter, learner.class, tune.measure) {
  tctrl = makeTuneControlRandom(maxit = TUNE_BUDGET)
  algoTemplate(static, iter, tctrl)
})

addAlgorithm(reg, id = "mbo", fun = function(static, iter, learner.class, tune.measure) {
  library(mlrMBO)
  mbo.ctrl = makeMBOControl(
    init.design.points = MBO_INIT_DES_SIZE,
    iters = MBO_ITERS,
    save.on.disk.at = integer(0L),
  )
  mbo.ctrl = setMBOControlInfill(mbo.ctrl,
    crit = MBO_INFILL_CRIT,
    opt.restarts = MBO_FOCUSSEARCH_RESTARTS,
    opt.focussearch.maxit = MBO_FOCUSSEARCH_ITERS,
    opt.focussearch.points = MBO_FOCUSSEARCH_POINTS
  )
  surrogate = makeLearner(MBO_SURROGATE)
  surrogate = setHyperPars(surrogate, par.vals = MBO_SURROGATE_VALS)
  surrogate = setPredictType(surrogate, "se")
  tctrl = mlr:::makeTuneControlMBO(learner = surrogate, mbo.control =  mbo.ctrl)
  algoTemplate(static, iter, tctrl)
})







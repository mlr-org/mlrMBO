library("mlrMBO")
# CRAN-Version of mlrMBO with print(y) behind y = maximize.mult * design[, control$y.name]
# in the funkcion makeMBOInfillCritEI 

set.seed(1)

f  = makeMultiObjectiveFunction(name = "",
  fn = function(x) c(-x, x^2),
  par.set = makeNumericParamSet("x", 1L, -10L, 10L),
  n.objectives = 2)
des = generateDesign(n = 5, par.set = getParamSet(f), fun = lhs::randomLHS)
surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern5_2")
setHyperPars(surr.km, multistart = 1)

control = makeMBOControl(n.objectives = 2L, propose.points = 1)
control = setMBOControlTermination(control, iters = 1)
control = setMBOControlMultiObj(control, method = "parego")
control = setMBOControlInfill(control, crit = makeMBOInfillCritEI(),
  opt.focussearch.maxit = 1, filter.proposed.points = FALSE)

run = mbo(f, design = des, learner = surr.km, control = control, show.info = FALSE)
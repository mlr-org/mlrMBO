library(BatchExperiments)
library(mlrMBO)
library(mco)
library(emoa)
library(devtools)

source("bench/mco/DTLZ.R")
source("bench/mco/LZ09.R")
source("bench/mco/UF.R")
source("bench/mco/WFG.R")
source("bench/mco/ZDT.R")

source("bench/mco/defs.R")

obj = lz8

nsga2Wrapper = function(static, generations) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  
  res = nsga2(static$objective, idim = getParamNr(par.set, devectorize = TRUE),
    odim = static$ny, lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = POPSIZE, generations = generations)
  pareto.set = setColNames(as.data.frame(subset(res$par, res$pareto.optimal, drop = FALSE)), 
    names.x)
  pareto.front = subset(res$value, res$pareto.optimal, drop = FALSE)
  hv = dominated_hypervolume(t(pareto.front), ref = static$ref)
  cd = crowding_distance(t(pareto.front))
  r2 = unary_r2_indicator(t(pareto.front), weights = WEIGHTS)
  list(pareto.set = pareto.set, pareto.front = pareto.front, hv = hv, cd = cd, r2 = r2)
}


paregoWrapper = function(static, prop.points) {
  par.set = static$par.set
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  
  learner = makeLearner("regr.km", predict.type = "se")
  
  iters = (FEVALS - INIT_DESIGN_POINTS) / prop.points
  ctrl = makeMBOControl(number.of.targets = static$ny, init.design.points = INIT_DESIGN_POINTS,
    iters = iters, propose.points = prop.points,
    save.on.disk.at = integer(0L))
  ctrl = setMBOControlInfill(ctrl, crit = "ei")#, opt.focussearch.points = 100, opt.restarts = 1L)
  ctrl = setMBOControlMultiCrit(ctrl)
  
  res = mbo(makeMBOFunction(static$objective), static$par.set,
    learner = learner, control = ctrl, show.info = TRUE)
  hv = dominated_hypervolume(t(res$pareto.front), ref = static$ref)
  cd = crowding_distance(t(res$pareto.front))
  r2 = unary_r2_indicator(t(res$pareto.front), weights = WEIGHTS)
  list(paretoset = res$pareto.set, pareto.front = res$pareto.front, hv = hv, cd = cd, r2 = r2)
}

static = list(
  objective = obj,
  par.set = makeNumericParamSet("x", len = 5L, lower = 0, upper = 1),
  ny = 2L,
  ref = c(11, 11)
)

res1 = paregoWrapper(static, 1)
res2 = nsga2Wrapper(static, 10)


res1
res2
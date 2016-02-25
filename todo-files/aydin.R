library(BBmisc)
library(mlrMBO)
library(parallelMap)
library(emoa)
source("todo-files/plot_multicritMBO.R")
set.seed(1)

# create parameter space
cost = makeNumericParam("cost", lower = -24, upper = 1, trafo = function(x) 2^x)
gamma =  makeNumericParam("gamma", lower = -24, upper = 1, trafo = function(x) 2^x)
epsDualGap = makeNumericParam("epsilon", lower = -13, upper = -1, trafo = function(x) 2^x)
ps= makeParamSet(cost, gamma, epsDualGap)

f = function(x) {
  t = exp( (1.0 - x$cost)^2 + ( 1.0 - x$gamma)^2 + (0.1 - x$epsilon)^2)
  return(c(t, (0.1 - x$eps)^2))
}

imputeFun = function(x, y, opt.path) {
  c(0.5 + rexp(1, 10), 60 * 480 + rexp(1, 1 / 100))
}

# create control object
learner = makeLearner("regr.km", predict.type = "se")
control = makeMBOControl(
  n.objectives = 2L,
  minimize = c(TRUE, TRUE),
  iters = 5L,
  propose.points = 20L,
  init.design.points = 20L,
  y.name = c("exp", "eps"),
#  save.on.disk.at = 1:11,
#  save.file.path =paste(storagePath, "/saveFile.mboData", sep = ""),
  impute = imputeFun)
control = setMBOControlInfill (control,
                               crit = "lcb",
                               filter.proposed.points = TRUE,
                               filter.proposed.points.tol = 1e-2)
control = setMBOControlMultiCrit (control,
                                  method = "parego",
                                  parego.use.margin.points = c(TRUE, FALSE))
resMBO = mbo(f, ps, learner = learner, control = control)
#plot(resMBO, infill.crit = "lcb")


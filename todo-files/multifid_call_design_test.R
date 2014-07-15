#mlrMBO workflow
#library("mlrMBO")
library("lhs")
library(devtools)
library("BBmisc")
library("ParamHelpers")
library("checkmate")
load_all(".")
options(warn = 2)

source("todo-files/test_functions.R")

par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 5),
  makeNumericParam("dw.perc", lower=0, upper=1)
)

objfun = makeNoiseFunction(makeShiftFunction(bakeFunction(hartman), direction = 2), sd.fac = 0.001)
  
control = makeMBOControl(
  init.design.points = 50L, #distributed over the different levels, seems not to work for <5 each
  init.design.fun = maximinLHS,
  iters = 20,
  on.learner.error = "stop",
  show.learner.output = FALSE,
)
control = setMBOControlInfill(control = control, 
                              crit = "multiFid", 
                              opt = "focussearch", 
                              opt.restarts = 1L, 
                              opt.focussearch.maxit = 1L, 
                              opt.focussearch.points = 300L)
control = setMBOControlMultiFid(control = control, 
                                param = "dw.perc", 
                                lvls = c(0.1, 0.3, 0.5, 1))
surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim = TRUE, jitter = TRUE)
surrogat.model = makeLearner("regr.randomForest", predict.type="se")
result = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control, show.info = TRUE)
pdf("multifid_steps_test.pdf", width=10, height=12)
for (i in seq_along(result$plot.data)) {
  plot = genGgplot(result$plot.data[[i]], title = sprintf("Step %i", i))
  print(plot)
}
dev.off()
as.data.frame(result$opt.path)
result$y.hat

### DEBUG ####

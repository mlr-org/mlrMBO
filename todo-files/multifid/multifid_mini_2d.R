library(devtools)
library(gridExtra)
load_all(".")
source("todo-files/test_functions.R")
options(warn = 2)
set.seed(030)
e.lvls = c(0.1, 0.3, 1)

ctrl = makeMBOControl(
  init.design.points = length(e.lvls) * 2 * 5, 
  init.design.fun = maximinLHS,
  iters = 4L,
  on.learner.error = "stop",
  show.learner.output = FALSE
)

ctrl = setMBOControlInfill(
  crit = "multiFid",
  control = ctrl, 
  opt = "focussearch", 
  opt.restarts = 1L, 
  opt.focussearch.maxit = 1L, 
  opt.focussearch.points = 100L,
  filter.proposed.points = TRUE,
  filter.proposed.points.tol = 0.001
)

ctrl = setMBOControlMultiFid(
  control = ctrl, 
  param = "dw.perc", 
  lvls = e.lvls,
  cor.grid.points = 40L,
  costs = NULL #c(0.1, 0.2, 3)
)

par.set = makeParamSet(
  makeNumericParam(id = "a", lower = 0, upper = 15),
  makeNumericParam(id = "b", lower = 0, upper = 15)
)

lrn = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)

obj = makeMBOMultifidFunction(addDistortion(addDistortion(hartman2d, g=noiseGaussian), g=yupp), lvls = ctrl$multifid.lvls)
res = mbo(fun = obj, par.set = par.set, control = ctrl, learner = lrn, show.info = TRUE)

pdf("../plots/tmp_multifid_min_2d.pdf", width = 9, height = 11)
for(i in seq_along(res$plot.data)) {
 plots = genGgplot2dRaw(plotdata = res$plot.data[[i]], subset.variable = c("y", "crit"), add.g = NULL)
 do.call(grid.arrange, c(plots, list(nrow = 1, main = sprintf("Stage %i", i))))
 # cat ("Press [enter] to continue")
 # line <- readline()
}
dev.off()


# ctrl2 = makeMBOControl(
#   init.design.points = length(e.lvls) * 5 * 2, 
#   init.design.fun = maximinLHS,
#   iters = 20L,
#   on.learner.error = "stop",
#   show.learner.output = FALSE
# )
# 
# ctrl2 = setMBOControlInfill(
#   crit = "aei",
#   control = ctrl2, 
#   opt = "focussearch", 
#   opt.restarts = 1L, 
#   opt.focussearch.maxit = 1L, 
#   opt.focussearch.points = 100L,
#   filter.proposed.points = TRUE,
#   filter.proposed.points.tol = 0.001
# )
# obj2 = function(x){
#   x$.multifid.lvl = length(e.lvls)
#   obj(x)
# }
# res2 = mbo(fun = obj2, par.set = par.set, control = ctrl2, learner = setPredictType(lrn, predict.type = "se"), show.info = TRUE)
# 
# obj(x = list(x1 = 0.114, x2 = 0.556, .multifid.lvl = 3))
# obj(x = list(x1 = res$x[1], x2 = res$x[2], .multifid.lvl = 3))
# obj(x = list(x1 = res2$x[1], x2 = res2$x[2], .multifid.lvl = 3))

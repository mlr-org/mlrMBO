#mlrMBO workflow
library("lhs")
library(devtools)
library("BBmisc")
library("ParamHelpers")
library("checkmate")
library("LiblineaR")
library("plyr")
library("OpenML")

load_all(".")
options(warn = 2)

set.seed(44137)

task.openML = downloadOpenMLTask(id = 2328)
openML.as.mlr = toMLR(task.openML)
task = openML.as.mlr$mlr.task

lrn1 = makeLearner("classif.LiblineaRBinary", type = 1)
lrn1.par.set = makeParamSet(
  makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x)
)
lrn2 = makeDownsampleWrapper(learner = lrn1, dw.stratify = TRUE)
lrn2.par.set = makeParamSet(
  makeNumericParam("dw.perc", lower=0, upper=1)
)
par.set = c(lrn1.par.set, lrn2.par.set)
rinst = openML.as.mlr$mlr.rin

makeObjFun = function(lrn, task, rsm = rinst) {
  force(lrn)
  force(task)
  force(rsm)
  #we could also implement a fixed holdout for each level here if provided par.set
  function(x) {
    # produce train test split manually. then downsample training further.
    lrn.local = setHyperPars(lrn, par.vals=x)
    y = resample(lrn.local, task, rsm, show.info=FALSE)$aggr[[1L]]
    return(y)
  }
}
objfun = makeObjFun(lrn2, task)
  
control = makeMBOControl(
  init.design.points = 20L, #distributed over the different levels, seems not to work for <5 each
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
                                lvls = c(0.1, 0.3, 0.5, 1),
                                costs = function(cur, last) (last / cur)^1.5)
surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim = TRUE, jitter = TRUE)
result = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control, show.info = TRUE)

# TUNE DISCRETE ####
ps.disc = makeParamSet(makeDiscreteParam("cost", values = 2^(-10:10)), 
                       makeDiscreteParam("dw.perc", values = 1))
ctrl = makeTuneControlGrid()
r = tuneParams(lrn2, task = task, resampling = rinst, 
               par.set = ps.disc, control = ctrl)
df = as.data.frame(r$opt.path)
df = rename(df, c("mmce.test.mean"="value")); df$variable = "response"
df$cost = log2(as.numeric(as.character(df$cost)))
df$dw.perc = 1
add.g = list(geom_line(data = df, alpha = 0.5, lty = 2),
             scale_color_gradient(low = "green", high = "blue"))


pdf("multifid_steps_liblinear_openml_cost.pdf", width=10, height=12)
for (i in seq_along(result$plot.data)) {
  plot = genGgplot(result$plot.data[[i]], title = sprintf("Step %i", i), add.g = add.g)
  print(plot)
}
dev.off()
as.data.frame(result$opt.path)
result$y.hat

### DEBUG ####

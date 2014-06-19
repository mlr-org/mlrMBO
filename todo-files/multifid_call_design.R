#mlrMBO workflow

library(devtools)
library("BBmisc")
library("ParamHelpers")
load_all(".")
options(warn = 2)

task = load2("../2013-ml_big_data_tuning/datasets/waveform5000.RData")
task = makeClassifTask(id="Sonar", data=getTaskData(task), target="class")

lrn1 = makeLearner("classif.ksvm")
lrn1.par.set = makeParamSet(
  makeNumericParam("sigma", lower = -15, upper = 5, trafo = function(x) 2^x)
)
lrn2 = makeDownsampleWrapper(learner=lrn1, dw.select="perc", dw.stratify=TRUE) #FIXEM TRUE does not work?
lrn2.par.set = makeParamSet(
  makeDiscreteParam("dw.val", values = c(0.1, 0.3, 1))
)
par.set = c(lrn1.par.set, lrn2.par.set)

makeObjFun = function(lrn, task, rsm = makeResampleDesc(method = "Holdout", split=2/3)) {
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
multiFid.control = makeMBOMultiFidControl(fid.param="dw.val")
control = makeMBOControl(
  init.design.points = 15, #distributed over the different levels
  init.design.fun = maximinLHS,
  iters = 10,
  on.learner.error = "stop",
  show.learner.output = FALSE,
  infill.crit = "multiFid",                   #### ?
  infill.opt = "focussearch",
  infill.opt.restarts = 1L,
  infill.opt.focussearch.maxit = 1L,
  infill.opt.focussearch.points = 900L, #distributed over the different levels
  multiFid.control = multiFid.control
)

surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim=TRUE)
result = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control, show.info = TRUE)
as.data.frame(result$opt.path)
result$y.hat

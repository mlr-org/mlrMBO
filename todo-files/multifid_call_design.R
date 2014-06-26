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
lrn2 = makeDownsampleWrapper(learner=lrn1, dw.stratify=TRUE) #FIXEM TRUE does not work?
lrn2.par.set = makeParamSet(
  makeNumericParam("dw.perc", lower=0, upper=1)
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
control = makeMBOControl(
  init.design.points = 9L, #distributed over the different levels
  init.design.fun = maximinLHS,
  iters = 10,
  on.learner.error = "stop",
  show.learner.output = FALSE,
)
control = setMBOControlInfill(control = control, crit = "multiFid", opt = "focussearch", opt.restarts = 1L, opt.focussearch.maxit = 1L, opt.focussearch.points = 300L)
control = setMBOControlMultiFid(control = control, multifid.param = "dw.perc", multifid.lvls = c(0.1, 0.3, 1))
surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim=TRUE)
result = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control, show.info = TRUE)
as.data.frame(result$opt.path)
result$y.hat

### DEBUG:
mf.learner = makeMultiFidLearner(surrogat.learner=surrogat.model, par.set=par.set, control=control)
mf.design = generateMBOMultiFidDesign(par.set=par.set, control=control)
oldopts = list(
  ole = getOption("mlr.on.learner.error"),
  slo = getOption("mlr.show.learner.output")
)
opt.path = makeOptPathDF(par.set, control$y.name, control$minimize,
                         include.error.message = FALSE,
                         include.exec.time = TRUE)
mbo.design = generateMBODesign(design=mf.design, fun=objfun, par.set=par.set, opt.path=opt.path, control=control, show.info=TRUE, oldopts=oldopts, more.args=list())
mf.model = train.MultiFidLearner(mf.learner, task=convertOptPathToTask(opt.path))
predict(mf.model, task=convertOptPathToTask(opt.path))
predict(mf.model, newdata=data.frame(sigma=2, dw.perc=0.2))
predict(mf.model$models[[3]], newdata=convertOptPathToDesign(opt.path)[,1,drop=F])

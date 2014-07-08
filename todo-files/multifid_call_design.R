#mlrMBO workflow
#library("mlrMBO")
library("lhs")
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
  init.design.points = 9L, #distributed over the different levels, seems not to work for <5 each
  init.design.fun = maximinLHS,
  iters = 10,
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
                                lvls = c(0.1, 0.3, 1))
surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim=TRUE)
result = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control, show.info = TRUE)
pdf("multifid_steps.pdf", width=6, height=6)
for (i in seq_along(result$plot.data)) {
  plot = genGgplot(result$plot.data[[i]], subset.variable=c("response", "crit"), title = sprintf("Step %i", i))
  print(plot)
}
dev.off()
as.data.frame(result$opt.path)
result$y.hat


stop()
### DEBUG: ####
mf.learner = makeMultiFidLearner(surrogat.learner=surrogat.model, par.set=par.set, control=control)
mf.design = mlrMBO:::generateMBOMultiFidDesign(par.set=par.set, control=control)
oldopts = list(
  ole = getOption("mlr.on.learner.error"),
  slo = getOption("mlr.show.learner.output")
)
opt.path = makeOptPathDF(par.set, control$y.name, control$minimize,
                         include.error.message = FALSE,
                         include.exec.time = TRUE)
mbo.design = mlrMBO:::generateMBODesign(design=mf.design, fun=objfun, par.set=par.set, opt.path=opt.path, control=control, show.info=TRUE, oldopts=oldopts, more.args=list())
mf.model = train.MultiFidLearner(mf.learner, task=mlrMBO:::convertOptPathToTask(opt.path))
predict(mf.model, task=mlrMBO:::convertOptPathToTask(opt.path))
predict(mf.model, newdata=data.frame(sigma=2, dw.perc=0.3))
predict(mf.model$models[[3]], newdata=convertOptPathToDesign(opt.path)[,1,drop=F])

## Crtieria
compound.model = mf.model

#subsets a data.frame to a given value of the fid.param
subsetOnPar = function(data, par.val){
  data = subset(data, data[[control$multifid.param]] == par.val)
}

# local needed functions
# calculate cost relation between model w/ par.val and last model
calcModelCost = function(par.val) {
  control$multifid.costs(par.val, tail(control$multifid.lvls, 1))
}

# estimate process noise tau of real process belonging to par.val, we use residual sd here
calcModelSD = function(par.val) {
  newdata = convertOptPathToDesign(opt.path)
  newdata = subsetOnPar(newdata, par.val)
  sqrt(estimateResidualVariance(compound.model, data = newdata, target = "y"))
}

# calculate GLOBAL correlation between model w/ par.val and last model. currently rank correlation.
calcModelCor = function(par.val, grid) {
  grid1 = grid; grid1[[control$multifid.param]] = par.val
  grid2 = grid; grid2[[control$multifid.param]] = tail(control$multifid.lvls, 1)
  p1 = predict(compound.model, newdata=grid1)$data$response
  p2 = predict(compound.model, newdata=grid2)$data$response
  # check whether vectors are constant, cor = NA then
  if (diff(range(p1)) < sqrt(.Machine$double.eps) || diff(range(p2)) < sqrt(.Machine$double.eps))
    0
  else
    max(cor(p1, p2, method="spearman"), 0)
}
corgrid = generateDesign(n=control$multifid.cor.grid.points, par.set=parSetWithout(par.set, control$multifid.param))

model.sd = vnapply(control$multifid.lvls, calcModelSD)
model.cor = vnapply(control$multifid.lvls, calcModelCor, grid=corgrid)
model.cost = vnapply(control$multifid.lvls, calcModelCost)

grid.design = generateGridDesign(par.set = parSetWithout(par.set, control$multifid.param), resolution = 20L)
grid.design = expandDesign(design = grid.design, control = control)
grid.design
infillCritMultiFid2(points=grid.design, model=compound.model, control=control, par.set=par.set, design=convertOptPathToDesign(opt.path), model.cor=model.cor, model.sd=model.sd, model.cost=model.cost)
cbind(predict(compound.model, newdata=grid.design)$data, grid.design)
cbind(predict(compound.model, newdata=convertOptPathToDesign(opt.path)), convertOptPathToDesign(opt.path))
a = convertOptPathToDesign(opt.path)[1,]
b = data.frame(sigma = round(a$sigma,15), dw.perc = 0.1)
predict(compound.model, newdata=b)$data

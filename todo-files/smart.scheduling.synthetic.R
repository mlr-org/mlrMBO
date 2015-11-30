#devtools::load_all("~/gits/mlr")
devtools::load_all()
library(BatchExperiments)

#suggested: Number of phsy. CPUs
k = 4
#suggested 30
init.design.points = 30
#suggested 70
iters = 20
#suggested 10
cv.iters = 2
#walltime in hours 48 or Inf
walltime = 8

unlink("~/dump/ss", recursive = TRUE, force = TRUE)

reg = makeExperimentRegistry(
  id = "mbo_scheduling",
  file.dir = "~/dump/ss",
  packages = c("mlrMBO", "parallelMap"),
  multiple.result.files = FALSE,
  seed = 54119
)


### Function to optimize
test.function.zhou2011 = function(x, category) {
  if(category == "a") return(cos(6.8*pi*(x/2)))
  if(category == "b") return(-cos(7*pi*(x/2)))
  if(category == "c") return(cos(7.2*pi*(x/2)))
}

test.function.swiler2014 = function(x, category) {
  if(category == "a") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2)
  if(category == "b") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2 + 0.5 * sin(2 * pi * x[2] - pi))
  if(category == "c") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2 + 0.8 * sin(2 * pi * x[2] - pi))
  if(category == "d") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2 + 8 * sin(2 * pi * x[2] - pi))
  if(category == "e") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2 + 3.5 * sin(2 * pi * x[2] - pi))
}

wait.function = function(x, category) {
  if(category == "a") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2)
  if(category == "b") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2 + 0.5 * sin(2 * pi * x[2] - pi))
  if(category == "c") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2 + 0.8 * sin(2 * pi * x[2] - pi))
  if(category == "d") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2 + 8 * sin(2 * pi * x[2] - pi))
  if(category == "e") return(sin(2 * pi * x[2] - pi) + 7 * sin(2 * pi * x[1] - pi)^2 + 3.5 * sin(2 * pi * x[2] - pi))
}

tf1 = function(x, sd = 0.5) {test.function.zhou2011(x$x, x$category) + rnorm(1, sd = sd)}

tf2 = function(x, sd = 0.5) {test.function.swiler2014(c(x$x1, x$x2), x$category) + rnorm(1, sd = sd)}

opt.funs = list(zhou2011 = tf1, swiler2014 = tf2)

par.sets = list(
  zhou2011 = makeParamSet(
    makeDiscreteParam("category", values = letters[1:3]), 
    makeNumericParam("x", lower = 0, upper = 1)),
  swiler2014 = makeParamSet(
    makeDiscreteParam("category", values = letters[1:5]), 
    makeNumericParam("x1", lower = 0, upper = 1), 
    makeNumericParam("x2", lower = 0, upper = 1))
)

mbo.ctrl = makeMBOControl(
  noisy = TRUE, 
  init.design.points = init.design.points,
  final.method = "best.true.y",
  propose.points = k,
  schedule.nodes = k, 
  time.budget = walltime*60^2*0.9,
  save.on.disk.at.time = 60*30,
  save.file.path = file.path(reg$file.dir,"mboState.RData"))

mbo.ctrl = setMBOControlInfill(mbo.ctrl,
  crit = "lcb",
  opt = "focussearch",
  crit.lcb.lambda = 2,
  opt.focussearch.maxit = 5,
  opt.focussearch.points = 1000)

mbo.ctrl = setMBOControlMultiPoint(mbo.ctrl, lcb.multiple = "random")

#rs, randomSearch
#r.lcb, randomLCB ohne scheduling
#r.s.lcb ~ mit scheduling
#r.s.time.lcb ~ mit Zeitsortierung
experiment.configurations = data.frame(
  propose.points = c(3*k, k, rep(3*k, 2)),
  iters = c(floor(iters/3), rep(iters, 3)),
  infill.crit = c("random", rep("lcb", 3)),
  multipoint.method = c("random", rep("lcb", 3)),
  schedule.method = c(rep("none", 2), rep("smartParallelMap", 2)),
  schedule.priority.time = c(rep(FALSE, 3), TRUE),
  config.name = c("rs", "r.lcb", "r.s.lcb", "r.s.time.lcb"),
  stringsAsFactors = FALSE
)

surrogate.learner = makeLearner("regr.randomForest", predict.type = "se", nr.of.bootstrap.samples = 20, ntree = 250)
surrogate.learner = makeImputeWrapper(surrogate.learner, classes = list(numeric = imputeConstant(10*2^15), factor = imputeConstant("NA"), integer = imputeConstant(10*2^15)))

mbo(opt.funs[[1]], par.set = par.sets[[1]], learner = surrogate.learner, control = mbo.ctrl)
### BatchExp Stuff ####
algoMBOWrapper = function(mbo.ctrl, surrogate.learner) {
  force(mbo.ctrl)
  force(surrogate.learner)
  function(job, static, dynamic, ...) {
    mbo.pars = list(...)
    for (ctrl.name in names(mbo.pars)) {
      mbo.ctrl[[ctrl.name]] = mbo.pars[[ctrl.name]]
    }
    mbo.ctrl$save.file.path = gsub(".RData", sprintf("_%i.RData", job$id) , mbo.ctrl$save.file.path)
    parallelStartMulticore(cpus = mbo.ctrl$schedule.nodes, level = "mlrMBO.feval")
    set.seed(123 + job$repl)
    or = mbo(fun = static$opt.fun, par.set = static$opt.par.set, learner = surrogate.learner, control = mbo.ctrl)
    parallelStop()
    return(or)
  }
}

## BatchExp Generate Stuff ####
pdes = lapply(names(opt.funs), function(opt.fun.name) {
  addProblem(reg = reg, id = opt.fun.name,
             static = list(opt.fun = opt.funs[[opt.fun.name]], opt.par.set = par.sets[[opt.fun.name]]),
             dynamic = function(job, static, sd) {invisible()},
             seed = reg$seed)
  makeDesign(id = opt.fun.name, design = data.frame(sd = c(0.1, 0.5)))
})

mbo.ctrls = list(mbo.ctrl = mbo.ctrl)
ades = lapply(names(mbo.ctrls), function(mbo.ctrl.name) {
  addAlgorithm(reg = reg, id = mbo.ctrl.name, fun = algoMBOWrapper(mbo.ctrl = mbo.ctrls[[mbo.ctrl.name]], surrogate.learner = surrogate.learner))
  makeDesign(id = mbo.ctrl.name, design = experiment.configurations)
})

addExperiments(reg, prob.designs = pdes, algo.designs = ades, repls = cv.iters, skip.defined = FALSE)

submit.ids = findExperiments(reg)
submitJobs(reg, ids = sample(submit.ids), resources = list(walltime = walltime*60^2, memory = 8000L, ppn = k*2))
waitForJobs(reg)

res.list = reduceResultsList(reg)

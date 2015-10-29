#devtools::load_all("~/gits/mlr")
#devtools::load_all()
library(mlrMBO)
library(parallelMap)
library(BatchExperiments)

reg = makeRegistry(
  id = "mbo_scheduling",
  file.dir = "~/mbo_scheduling/",
  packages = c("mlr", "mlrMBO", "parallelMap"),
  multiple.result.files = FALSE,
  seed = 54119
)
#

#task einlesen
libsvm.read = function(file) {
  library("e1071")
  library("Matrix")
  dataset = read.matrix.csr(file)
  colNames = sapply( (1:(dim(dataset$x)[2])), FUN = function(x) { paste("X",x, sep = "") })
  dataframe = as.data.frame(as.matrix(dataset$x))
  colnames(dataframe) = colNames
  dataframe$Y = dataset$y
  dataframe
}
a9a = libsvm.read(file = "../data/a9a")
task = makeClassifTask(id = "a9a", data = a9a, target = "Y")

learner = makeModelMultiplexer(list(
  makeLearner("classif.randomForest", id = "classif.randomForest"),
  makeLearner("classif.svm", id = "classif.svm.radial", kernel = "radial"),
  makeLearner("classif.fnn", id = "classif.fnn") #as soon as I add this (even without the ParamSet) it generates the generateDesign warning -- seems right
))

ps.hp1 = makeModelMultiplexerParamSet(
  learner, 
  classif.svm.radial = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2^x),
    makeNumericParam("gamma", lower = -15, upper = 15, trafo = function(x) 2^x)),
  classif.randomForest = makeParamSet(
    makeIntegerParam("mtry", lower = floor((getTaskNFeats(task)^1/4)), upper = ceiling((getTaskNFeats(task))^1/1.5)),
    makeIntegerParam("nodesize", lower = 1, upper = 10)),
  classif.fnn = makeParamSet(
    makeIntegerParam("k", lower = 1, upper = 10)
  )
)


mbo.ctrl = makeMBOControl(
  noisy = TRUE, 
  iters = 50, 
  init.design.points = 30,
  final.method = "best.predicted",
  propose.points = 30L,
  schedule.nodes = 7L,
  schedule.method = "smartParallelMap")

mbo.ctrl = setMBOControlInfill(
  mbo.ctrl, crit = "lcb",
  opt = "focussearch",
  opt.focussearch.maxit = 3,
  opt.focussearch.points = 1000)

schedule.methods = c("none", rep("smartParallelMap", times = 4))
schedule.priorities = c("infill", "infill", "explore", "exploit", "balanced")

surrogate.learner = makeLearner("regr.randomForest", predict.type = "se", nr.of.bootstrap.samples = 20, nodesize = 2)
surrogate.learner = makeImputeWrapper(surrogate.learner, classes = list(numeric = imputeConstant(10*2^15), factor = imputeConstant("NA"), integer = imputeConstant(10*2^15)))

mlr.ctrl = mlr:::makeTuneControlMBO(same.resampling.instance = FALSE, learner = surrogate.learner, mbo.control = mbo.ctrl, mbo.keep.result = TRUE, continue = TRUE)

doExperiment = function(schedule.method, schedule.priority, mlr.ctrl, learner, ps.hp1, mbo.ctrl, task) {
  this.mlr.ctrl = mlr.ctrl
  if (schedule.method == "none") {
    this.mlr.ctrl$mbo.control$propose.points = this.mlr.ctrl$mbo.control$schedule.nodes
  }
  this.mlr.ctrl$mbo.control$schedule.method = schedule.method
  this.mlr.ctrl$mbo.control$schedule.priority= schedule.priority
  
  inner.rdesc = makeResampleDesc("Holdout")
  outer.rdesc = makeResampleDesc("CV", iter = 3)
  
  lrn.tuned = makeTuneWrapper(learner, inner.rdesc, par.set = ps.hp1, control = this.mlr.ctrl, show.info = FALSE)
  parallelStartMulticore(cpus = mbo.ctrl$schedule.nodes, level = "mlrMBO.feval")
  #options(mlr.debug.seed = 123L)
  #set.seed(1)
  res = resample(lrn.tuned, task, outer.rdesc, measures = list(mmce, timetrain), extract = getTuneResult)
  parallelStop()
  return(res)
}

batchMap(reg, fun = doExperiment, schedule.method = schedule.methods, schedule.priority = schedule.priorities, more.args = list(mlr.ctrl = mlr.ctrl, learner = learner, ps.hp1 = ps.hp1, mbo.ctrl = mbo.ctrl, task = task))

submitJobs(reg, resources = list(walltime = 48*60^2, memory = 8000L, queue = "long_quad", ppn = 8))
waitForJobs(reg)


stop("finished until here")

### lokal einlesen

library(data.table)
library(BatchExperiments)
reg = loadRegistry("~/lido/mbo_scheduling/")

reducer = function(job, res) {
  is = seq_row(res$measures.test)
  r.dfs = lapply(is, function(i) {
    tune.y = c(tune.y = res$extract[[i]]$mbo.result$y)
    test.y = res$measures.test[i, -1]
    x = res$extract[[i]]$x
    names(x) = paste0("x.", names(x))
    names(test.y) = paste0("test.", names(test.y))
    op.dt = as.data.table(res$extract[[i]]$mbo.result$opt.path)
    cbind(iter = res$measures.test[i, "iter"], op.dt, tune.y, test.y,
          do.call(cbind, x),
          schedule.method = res$extract[[i]]$mbo.result$control$schedule.method,
          schedule.nodes = res$extract[[i]]$mbo.result$control$schedule.nodes,
          schedule.priority = res$extract[[i]]$mbo.result$control$schedule.priority
    )
  })
  r.dfs = rbindlist(r.dfs)
  cbind(task = res$task.id, r.dfs)
}
res = reduceResultsList(reg, fun = reducer)
res.dt = rbindlist(res)

res.last.dt = res.dt[, cbind(.SD[.N, ], runs = .N), by = .(iter, task, schedule.method, schedule.priority)]

g = ggplot(res.last.dt, aes(x = schedule.priority, fill = schedule.priority, y = test.mmce))
g + geom_boxplot()

g = ggplot(res.last.dt, aes(x = schedule.priority, fill = schedule.priority, y = tune.y))
g + geom_boxplot()

g = ggplot(res.last.dt, aes(x = test.timetrain, y = test.mmce))
g + geom_point(aes(color = schedule.priority), size = 8) + geom_text(aes(label = iter))

g = ggplot(res.last.dt, aes(x = runs, y = test.mmce))
g + geom_vline(xintercept = 30 + 7 * 50) + geom_point(aes(color = schedule.priority, shape = x.selected.learner), size = 8) + geom_text(aes(label = iter))

g = ggplot(res.dt[dob != 0, ], aes(x = schedule.priority, fill = selected.learner))
g + geom_hline(yintercept = 7*50) + geom_bar() + facet_grid(~iter) + ylab("runs")

g = ggplot(res.dt, aes(x = dob, y = y, color = selected.learner))
g + geom_point() + facet_grid(schedule.priority~iter)

gph = function(sd) {
  diffs = sd$predicted.time - sd$exec.time
  qs = c(0,.1,.5,.9,1)
  quants = quantile(diffs, qs, na.rm = TRUE)
  names(quants) = c("min","q10","median","q90","max")
  c(list(mean.diff = mean(diffs),
         sd.diff = sd(diffs),
         dob.n = nrow(sd)),
    as.list(quants))
}

res.dob = res.dt[, gph(.SD), by = .(iter, task, schedule.method, schedule.priority, dob, selected.learner)]
g = ggplot(res.dob, aes(x = dob, y = mean.diff, color = selected.learner))
g + geom_pointrange(aes(x = dob.n, y = median, ymin = q10, ymax = q90)) + facet_grid(schedule.priority~iter)

g = ggplot(res.dt[, list(master.diff = (exec.time[1]-exec.time[-1])), by = .(iter, task, schedule.method, schedule.priority, dob, selected.learner)], aes(x = dob, y = master.diff, color = selected.learner))
g + geom_point() + facet_grid(schedule.priority~iter)

trailingMin = function(x, min.x = Inf) {
  for (i in seq_along(x))
    if (x[i] < min.x) {min.x = x[i]} else {x[i] = min.x}
  x
}

g.dt = res.dt[, list(max.exec = sum(exec.time), min.y = min(y)) , by = .(iter, task, schedule.method, schedule.priority, dob)]
g.dt = g.dt[, list(cum.max.exec = cumsum(max.exec), min.y = trailingMin(min.y)), by = .(iter, task, schedule.method, schedule.priority)]
g = ggplot(g.dt, aes(x = cum.max.exec, y = min.y, color = schedule.priority))
g + geom_line() + facet_grid(.~iter)

manualLoadBalancing = function(times, nodes = 7) {
  scheduled.at = numeric(length(times))
  scheduled.on = numeric(length(times))
  slots = numeric(nodes)
  for (i in seq_along(times)) {
    ind = which.min(slots)
    scheduled.on[i] = ind
    scheduled.at[i] = slots[ind]
    slots[ind] = slots[ind] + times[i]
  }
  data.table(scheduled.on = scheduled.on, scheduled.at = scheduled.at)  
}

res.scheduled = res.dt[, cbind(.SD, manualLoadBalancing(exec.time)), by = .(iter, task, schedule.method, schedule.priority, dob)]
dob.maxtime = res.scheduled[, list(slot.maxtime = max(.SD[, sum(exec.time) , by = scheduled.on])), by = .(iter, task, schedule.method, schedule.priority, dob)]
dob.maxtime[, slot.endtime := cumsum(slot.maxtime), by = .(iter, task, schedule.method, schedule.priority)]
setkey(res.scheduled, iter, task, schedule.method, schedule.priority, dob)
res.scheduled = res.scheduled[dob.maxtime]
g = ggplot(res.scheduled[dob > 0,], aes(y = scheduled.on, x = slot.endtime - slot.maxtime + scheduled.at, yend = scheduled.on, xend = slot.endtime - slot.maxtime + scheduled.at + exec.time)) ###HERE WORK ON
g + geom_segment(aes(color = selected.learner), size = 2) + facet_grid(schedule.priority~iter, scales = "free") + geom_vline(aes(xintercept = slot.endtime), alpha = 0.1)
### Bilder machen



op.dfs = lapply(extractSubList(res$extract, c("mbo.result", "opt.path"), simplify = FALSE), as.data.frame)
op.dfs.unsmart = lapply(extractSubList(res.unsmart$extract, c("mbo.result", "opt.path"), simplify = FALSE), as.data.frame)

plot(trailingMin(as.data.frame(res$extract[[2]]$mbo.result$opt.path)$y), type = "l", ylim = c(0.14, 0.2), xlim = c(0, 30))
lines(trailingMin(as.data.frame(res.unsmart$extract[[2]]$mbo.result$opt.path)$y), col = "blue")
save.image("smart.scheduling.RData")



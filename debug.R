all.res[[1]]$learner.id

res = all.res[extractSubList(all.res, c("learner.id")) == "regr.svm.mlrMBO"]
r = res[[1]]
r$performance
sum(getOptPathExecTimes(r$opt.result$opt.path))
op = (res[[1]]$opt.result$opt.path)


tune.rres2$mlrMBO.l$op.dfs[[1]]

tune.rres2$mlrMBO$op.dfs[[1]]

tail(tune.rres2$RandomSearch.l$op.dfs[[1]])

length(tune.rres2$RandomSearch.l$exec.times)

length(tune.rres2$RandomSearch.l$op.dfs)

tune.rres2$mlrMBO.l$exec.times

learner.id = "regr.svm"
task.id = "puma32H"
tune.rres2 = giveMeDataForVisuals(res.by.resampling, learner.id)
tune.rres2.full = giveMeDataForVisualsFull(res.by.resampling, learner.id)
giveMeTimeVsPerformance(c(tune.rres2, tune.rres2.full), init.design.points = init.design.points, title = "common.title")
tune.rres2$mlrMBO.l$op.dfs[[1]]
tune.rres2.full$mlrMBO.l.full$op.dfs[[1]]

tune.rres2$mlrMBO.l$op.steps
tune.rres2$mlrMBO.l$measures.test
tune.rres2.full$mlrMBO.l.full$measures.test
tune.rres2$RandomSearch.l$op.steps


library(mlr)
library(mlrMBO)
library(checkmate)
task = iris.task
resampling.inner = makeResampleDesc("Holdout")
resampling.outer = makeResampleDesc("CV", iters = 3L)
lrn = makeLearner("classif.svm")
lrn.s = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)
ps = makeParamSet(makeNumericParam(id = "cost", lower = -10, upper = 5, trafo = function(x) 2^x))
lrn.d = makeDownsampleWrapper(lrn, dw.perc = 0.1, dw.stratify = FALSE)
ctrl.mbo = makeMBOControl(iters = 10L, init.design.points = 10L)
#ctrl.mbo = setMBOControlInfill(control = ctrl.mbo, crit = "multiFid")
ctrl = mlr:::makeTuneControlMBO(learner = lrn.s, mbo.control = ctrl.mbo)
#ctrl = makeTuneControlRandom(maxit = NULL, exec.time.budget = 1, final.dw.perc = 1)
lrn.t = makeTuneWrapper(learner = lrn.d, resampling = resampling.inner, measures = list(mmce, timeboth), par.set = ps, control = ctrl, show.info = TRUE)

system.time({m = train(lrn.t, task = task)})
m$learner.model$opt.result
m$time
#63 sek
#48 sek
as.data.frame(m$learner.model$opt.result$opt.path)
#17
sum(getOptPathExecTimes(m$learner.model$opt.result$opt.path))
sum(m$learner.model$opt.result$opt.path$env$path$timetrain.test.mean)
sum(m$learner.model$opt.result$opt.path$env$path$timepredict.test.mean)
#0.536

mean(getOptPathExecTimes(m$learner.model$opt.result$opt.path) - getOptPathExecTimes(xx.opt.path))

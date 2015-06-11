library(mlr)
library(mlrMBO)
library(mlbench)
library(ggplot2)
library(reshape2)
resampling.inner = makeResampleDesc("Holdout")
lrn = makeLearner("classif.svm")
lrn.s = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)
ps = makeParamSet(makeNumericParam(id = "cost", lower = -10, upper = 5, trafo = function(x) 2^x))

lrn.d.100 = makeDownsampleWrapper(lrn, dw.perc = 1, dw.stratify = TRUE)
lrn.d.10 = makeDownsampleWrapper(lrn, dw.perc = 0.1, dw.stratify = TRUE)

ctrl.rnd = makeTuneControlRandom(maxit = 20L)
# ctrl.rnd$extra.args$dw.steps = 3L
ctrl.mbo = mlr:::makeTuneControlMBO(
  learner = lrn.s,
  mbo.control = makeMBOControl(iters = 10L, init.design.points = 10L))
# ctrl.mbo$extra.args$dw.steps = 3L

lrns = list(
  rnd.full = makeTuneWrapper(learner = lrn.d.100, resampling = resampling.inner, par.set = ps, control = ctrl.rnd, show.info = TRUE),
  rnd.dwn = makeTuneWrapper(learner = lrn.d.10, resampling = resampling.inner, par.set = ps, control = ctrl.rnd, show.info = TRUE),
  mbo.full = makeTuneWrapper(learner = lrn.d.100, resampling = resampling.inner, par.set = ps, control = ctrl.mbo, show.info = TRUE),
  mbo.dwn = makeTuneWrapper(learner = lrn.d.10, resampling = resampling.inner, par.set = ps, control = ctrl.mbo, show.info = TRUE)
  )

for (i in names(lrns)) {
  lrns[[i]]$id = i
}

timeexec = makeMeasure(
  id = "timeexec", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "regr", "surv", "costsens", "cluster", "req.model"),
  name = "Time of executing the fitnes function",
  fun = function(task, model, pred, feats, extra.args) {
    sum(getOptPathExecTimes(model$learner.model$opt.result$opt.path))
  }
)

#oversamplings = c(100,200)
oversamplings = 100 * 2^(0:9)
tasks = lapply(oversamplings, function(x) {
  df = as.data.frame(mlbench::mlbench.cassini(n = x))
  makeClassifTask(id = paste0("mlb", x), data = df, target = "classes")
})
res = benchmark(learners = lrns[1:4], tasks = tasks, measures = list(timetrain, timepredict, timeexec), resamplings = makeResampleDesc("Holdout"))

res.df = as.data.frame(res)
res.df$task.id = as.numeric(substr(as.character(res.df$task.id),4,999))
mdf = melt(res.df, id.vars = c("learner.id", "task.id"))
g = ggplot(mdf, aes(x = task.id, y = value, color = learner.id))
g = g + geom_line() + geom_point()
g = g + facet_wrap(~variable, scales = "free_y")
g = g + scale_x_log10()
ggsave(g, filename = "dwNmlr.pdf")
#g

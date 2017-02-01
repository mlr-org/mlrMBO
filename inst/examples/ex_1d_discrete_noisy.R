##### optimizing 1D fun with 3 categorical level and
##### noisy outout with random forest

library(mlrMBO)
library(ggplot2)
library(smoof)
set.seed(1)
configureMlr(show.learner.output = FALSE)
pause = interactive()

obj.fun = makeSingleObjectiveFunction(
  name = "Mixed decision space function",
  fn = function(x) {
    if (x$foo == "a") {
      return(5 + x$bar^2 + rnorm(1))
    } else if (x$foo == "b") {
      return(4 + x$bar^2 + rnorm(1, sd = 0.5))
    } else {
      return(3 + x$bar^2 + rnorm(1, sd = 1))
    }
  },
  par.set = makeParamSet(
    makeDiscreteParam("foo", values = letters[1:3]),
    makeNumericParam("bar", lower = -5, upper = 5)
  ),
  has.simple.signature = FALSE, # function expects a named list of parameter values
  noisy = TRUE
)
obj.fun = convertToMaximization(obj.fun)

ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 10L)

# we can basically do an exhaustive search in 3 values
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCriterionCB(),
  opt.restarts = 1L, opt.focussearch.points = 3L, opt.focussearch.maxit = 1L)

design = generateDesign(20L, getParamSet(obj.fun), fun = lhs::maximinLHS)

lrn = makeMBOLearner(ctrl, obj.fun)

newExampleRun = function(fun, design = NULL, learner = NULL, control, show.info = FALSE, more.args = NULL) {
  control.mod = control
  control.mod$store.model.at = 1:1024L
  mbo.res = mbo(fun = fun, design = design, learner = learner, control = control.mod, show.info = show.info, more.args = more.args)
  list(mbo.res = mbo.res, obj.fun = fun, control = control)
}

run = newExampleRun(obj.fun, design = design, learner = lrn, control = ctrl)

resolution = 400

# start plot
obj.fun = run$obj.fun
obj.fun.mean = coalesce(smoof::getMeanFunction(obj.fun), obj.fun)
par.set = getParamSet(run$obj.fun)
control = run$control
draw.design = generateGridDesign(par.set = par.set, resolution = resolution, trafo = FALSE)
draw.design$y.real = vnapply(dfRowsToList(draw.design, par.set = par.set), obj.fun.mean)

iter = 1
draw.design.iter = draw.design
# predict the mean of the outcome
this.model = run$mbo.res$models[[iter]]
prediction = predict(this.model, newdata = draw.design.iter)
draw.design.iter$y.predict = getPredictionResponse(prediction)
draw.design.iter$y.se = getPredictionSE(prediction)
# get the infill crit value
infill.fun = c

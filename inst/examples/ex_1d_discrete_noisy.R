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
  global.opt.value = 3,
  has.simple.signature = FALSE, # function expects a named list of parameter values
  noisy = TRUE
)

ctrl = makeMBOControl(propose.points = 1)
ctrl = setMBOControlMultiPoint(ctrl, method = "cb")
ctrl = setMBOControlTermination(ctrl, iters = 10L)

# we can basically do an exhaustive search in 3 values
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCriterionEI(),
  opt.restarts = 1L, opt.focussearch.points = 3L, opt.focussearch.maxit = 1L)

design = generateDesign(20L, getParamSet(obj.fun), fun = lhs::maximinLHS)

lrn = makeMBOLearner(ctrl, obj.fun)

newExampleRun = function(fun, design = NULL, learner = NULL, control, show.info = FALSE, more.args = NULL) {
  control.mod = control
  control.mod$store.model.at = 1:1024L
  mbo.res = mbo(fun = fun, design = design, learner = learner, control = control.mod, show.info = show.info, more.args = more.args)
  list(mbo.res = mbo.res, obj.fun = fun)
}

run = newExampleRun(obj.fun, design = design, learner = lrn, control = ctrl)

resolution = 20

# start plot
obj.fun = run$obj.fun
obj.fun.mean = coalesce(smoof::getMeanFunction(obj.fun), obj.fun)
par.set = getParamSet(run$obj.fun)
control = run$mbo.res$control
draw.design = generateGridDesign(par.set = par.set, resolution = resolution, trafo = FALSE)
draw.design$y.real = vnapply(dfRowsToList(draw.design, par.set = par.set), obj.fun.mean)
x.names = getParamIds(run$mbo.res$opt.path$par.set)
x.types = getParamTypes(run$mbo.res$opt.path$par.set)
x.name.numeric = x.names[x.types == "numeric"]
x.name.discrete = x.names[x.types != "numeric"]
y.names = run$mbo.res$opt.path$y.names

# opt path to data.frame
op.df = as.data.frame(run$mbo.res$opt.path)

# for one iteration
iter = 3
draw.design.iter = draw.design
# predict the mean of the outcome
this.model = run$mbo.res$models[[iter]]
this.design = op.df[this.model$subset, this.model$features]
prediction = predict(this.model, newdata = draw.design.iter)
draw.design.iter$y.predict = getPredictionResponse(prediction)
draw.design.iter$y.se = getPredictionSE(prediction)
# get the infill crit value
infill.crit = control$infill.crit$fun(points = draw.design.iter, models = list(this.model), control = control, par.set = par.set, design = this.design, iter = iter, attributes = TRUE)
draw.design.iter$infill.value = setAttribute(infill.crit, "crit.components", NULL)
draw.design.iter = cbind(draw.design.iter, attr(infill.crit, "crit.components"))

# opt path for this iteration
this.op.df = op.df[op.df$dob <= iter, ]
this.op.df$variable = y.names[1]
this.op.df$state = ifelse(this.op.df$dob == 0, "init", ifelse(this.op.df$dob == iter, "prop", "seq"))
# melt data for plotting
mdata = reshape2::melt(draw.design.iter, measure.vars = c("y.predict", "infill.value"))
# rename the melted columns according to input
mdata$variable = plyr::revalue(mdata$variable, replace = c(y.predict = y.names[1], infill.value = control$infill.crit$id))
mdata$variable = factor(mdata$variable, levels = c(y.names[1], control$infill.crit$id))
this.op.df$variable = factor(this.op.df$variable, levels = c(y.names[1], control$infill.crit$id))
# put name of discrete vector in front (like foo: a, foo: b, ...)
mdata[[x.name.discrete]] = factor(mdata[[x.name.discrete]], labels = sprintf("%s: %s", x.name.discrete, levels(mdata[[x.name.discrete]])))
this.op.df[[x.name.discrete]] = factor(this.op.df[[x.name.discrete]], labels = sprintf("%s: %s", x.name.discrete, levels(this.op.df[[x.name.discrete]])))

g = ggplot()
g = g + geom_line(data = mdata, mapping = aes_string(x = x.name.numeric, y = "value"))
g = g + geom_line(data = mdata[mdata$variable == y.names[1],], mapping = aes_string(x = x.name.numeric, y = "y.real"), linetype = 2)
g = g + geom_point(data = this.op.df, mapping = aes_string(x = x.name.numeric, y = y.names, shape = "state", color = "state"), size = 3)
g = g + facet_grid(as.formula(sprintf("%s~%s", "variable", x.name.discrete)), scales = "free")
g = g + theme(axis.title.y = element_blank())
g + labs(title = sprintf("%s, Iteration: %s", getName(run$obj.fun), iter), subtitle = "bla")


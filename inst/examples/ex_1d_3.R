##### optimizing 1D fun with 3 categorical level and
##### noisy outout with random forest

library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)
library(ggplot2)
library(grid)
library(gridExtra)

load_all(".", reset=TRUE)

configureMlr(show.learner.output=FALSE)

objfun = function(x) {
  if (x$foo == "a") {
    return(5 + rnorm(1))
  } else if (x$foo == "b") {
    return(4 + rnorm(1, sd=0.5))
  } else {
    return(3 + rnorm(1, sd=1))
  }
}

ps = makeParamSet(
  makeDiscreteParam("foo", values = c("a", "b", "c"))
)


ctrl = makeMBOControl(init.design.points=20, iters=5, infill.crit="ei",                    
  infill.opt.random.points=100, noisy=TRUE)

lrn = makeLearner("regr.randomForest", predict.type="se")

run = exampleRun(objfun, par.set = ps, learner=lrn, control=ctrl, points.per.dim=50)

print(run)

autoplot(run, pause=TRUE, densregion=TRUE)


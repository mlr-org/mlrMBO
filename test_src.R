library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)

load_all(".", reset=TRUE)

configureMlr(show.learner.output=FALSE)

set.seed(2)

objfun = function(x) {
  (x$x1 == "a") + ifelse(isScalarNA(x$x3), 30, (x$x3 - 15)^2)
}

ps = makeParamSet(
  makeDiscreteParam("x1", values=c("a", "b")),
  makeNumericVectorParam("x2", len=2, lower=1, upper=2),
  makeIntegerParam("x3", lower=10L, upper=20L, requires=quote(x1=="a")),
  makeDiscreteParam("x4", values=list(v=iris, w="123"), requires=quote(x1=="b"))
)

ctrl = makeMBOControl(minimize=TRUE, init.design.points=6, iters=5, propose.points=1, 
  infill.crit="ei", infill.opt="random", infill.opt.restarts=3L,
  infill.opt.random.maxit=5, infill.opt.random.points=1000L)

lrn = makeLearner("regr.randomForest", predict.type="se", fix.factors=TRUE)

res = mbo(objfun, ps, learner=lrn, control=ctrl)


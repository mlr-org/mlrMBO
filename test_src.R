library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)

load_all(".", reset=TRUE)

set.seed(6)

f = function(x) sin(x)

ps = makeNumericParamSet(len=1, lower=3, upper=13)

ctrl = makeMBOControl(minimize=TRUE, init.design.points=4, iters=5,
  infill.crit="ei")

lrn = makeLearner("regr.km", predict.type="se")

# run = exampleRun(makeMBOFunction(f), ps, control = ctrl, learner = lrn)

for (i in 1:5) {
  png(sprintf("~/Desktop/%i.png", i))
  plot(run, iter=i, pause=FALSE)
  dev.off()
}

# res = mbo(makeMBOFunction(f1), ps1, learner=lrn, control=ctrl)

# configureMlr(show.learner.output=FALSE)

# set.seed(2)

# objfun = function(x) {
  # (x$x1 == "a") + ifelse(isScalarNA(x$x3), 30, (x$x3 - 15)^2)
# }

# ps = makeParamSet(
  # makeDiscreteParam("x1", values=c("a", "b")),
  # makeNumericVectorParam("x2", len=2, lower=1, upper=2),
  # makeIntegerParam("x3", lower=10L, upper=20L, requires=quote(x1=="a")),
  # makeDiscreteParam("x4", values=list(v=iris, w="123"), requires=quote(x1=="b"))
# )

# ctrl = makeMBOControl(minimize=TRUE, init.design.points=6, iters=5, propose.points=1, 
  # infill.crit="ei", infill.opt="random", infill.opt.restarts=3L,
  # infill.opt.random.maxit=5, infill.opt.random.points=1000L)

# lrn = makeLearner("regr.randomForest", predict.type="se", fix.factors=TRUE)

# res = mbo(objfun, ps, learner=lrn, control=ctrl)


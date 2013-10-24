##### optimizing branin in 2D with multipoint proposal #####

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

objfun = generate_branin_function()

ctrl = makeMBOControl(init.design.points=10, iters=10, propose.points=5, 
  multipoint.method="multicrit",
  multipoint.multicrit.objective="ei.dist",
  multipoint.multicrit.dist="nearest.neighbor",
  multipoint.multicrit.maxit=200
)


lrn = makeLearner("regr.km", predict.type="se", covtype="matern3_2")

run = exampleRun(objfun, learner=lrn, control=ctrl, points.per.dim=50)

print(run)

autoplot(run, pause=TRUE)


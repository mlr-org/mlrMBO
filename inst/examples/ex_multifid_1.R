load_all()
source("todo-files/test_functions.R")

set.seed(123)
pause = interactive()

# objective function and param regions
objfun = addDistortion(sasena, yshift)
ps = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 10)
)
lvls = c(0.1, 0.5)
objfun2 = makeMBOMultifidFunction(objfun, lvls)

# define mlr learner
lrn = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)

# set control options
ctrl = makeMBOControl(init.design.points = 20L, iters = 20L,
  init.design.fun = maximinLHS, show.learner.output = FALSE
)
ctrl = setMBOControlInfill(ctrl, crit = "multiFid", opt = "focussearch",
  opt.restarts = 1L, opt.focussearch.maxit = 1L, opt.focussearch.points = 100L,
  filter.proposed.points = TRUE, filter.proposed.points.tol = 0.001
)
ctrl = setMBOControlMultiFid(ctrl, param = "lv", lvls = lvls, cor.grid.points = 20L,
  costs = function(cur, last) (last / cur)^1.2
)

# run optimizer
res = mbo(fun = objfun2, par.set = ps, learner = lrn, control = ctrl)


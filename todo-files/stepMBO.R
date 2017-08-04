# step mlrMBO
devtools::load_all()

fun = makeBraninFunction()
ps = getParamSet(fun)
control = makeMBOControl()
des = generateDesign(10, ps)
des$y = apply(des, 1, fun)

## intialize mbo
dummy.fun = makeSingleObjectiveFunction(name = "dummy", fn = function(...) return(NA), par.set = getParamSet(fun), minimize = shouldBeMinimized(fun), noisy = isNoisy(fun))
learner = mlrMBO::checkLearner(learner = NULL, control = control, fun = dummy.fun)
control$noisy = isNoisy(fun)
control$minimize = shouldBeMinimized(fun)
control = mlrMBO::checkStuff(fun = dummy.fun, design = des, learner = learner, control = control)
control$infill.crit = initCrit(control$infill.crit, fun, design, learner, control)
opt.problem = makeOptProblem(fun = dummy.fun, design = des, learner = learner, control = control)
opt.state = makeOptState(opt.problem)
evalMBODesign.OptState(opt.state)
finalizeMboLoop(opt.state)

## propose first point
proposePoints.OptState(opt.state)

## manual evaluation
x = data.frame(x1 = -3.5, x2 = 12)
y = fun(x = x)

## feedback to MBO
## determine extra information at x
prop = list(prop.points = x)
control$infill.crit
getExtras(n = nrow(prop$prop.points), prop = prop, train.time = 0, control = control)
addOptPathEl(op = getOptStateOptPath(opt.state), x = x, y = y, extra = list(insert.timestamp = as.integer(Sys.time())))



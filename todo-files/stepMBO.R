# step mlrMBO
devtools::load_all()

fun = makeBraninFunction()
ps = getParamSet(fun)
control = makeMBOControl()
des = generateDesign(10, ps)
des$y = apply(des, 1, fun)

## intialize mbo
opt.state = initSMBO(par.set = ps, design = des, control = control)

## propose first point
proposePoints(opt.state)

## manual evaluation
x = data.frame(x1 = -3.5, x2 = 12)
y = fun(x = x)

## feedback to MBO
feedSMBO(opt.state, x = x, y = y)


## propose next point
proposePoints(opt.state)

## manual evaluation
x = data.frame(x1 = 0, x2 = 0)
y = fun(x = x)

#' @inheritParams

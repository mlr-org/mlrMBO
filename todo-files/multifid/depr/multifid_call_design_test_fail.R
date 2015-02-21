#mlrMBO workflow
#library("mlrMBO")
library("lhs")
library(devtools)
library("BBmisc")
library("ParamHelpers")
library("checkmate")
load_all(".")
options(warn = 2)

set.seed(1)

source("todo-files/test_functions.R")

par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 2),
  makeNumericParam("dw.perc", lower=0, upper=1)
)

objfun = makeShiftFunction(makeAddFunction(fun=bakeFunction(hartman), addfun=uppMove, fac = 0.2), direction = 0.4)

control = makeMBOControl(
  init.design.points = 20L, #distributed over the different levels, seems not to work for <5 each
  init.design.fun = maximinLHS,
  iters = 20,
  on.learner.error = "stop",
  show.learner.output = FALSE,
)
control = setMBOControlInfill(control = control, 
                              crit = "multiFid", 
                              opt = "focussearch", 
                              opt.restarts = 1L, 
                              opt.focussearch.maxit = 1L, 
                              opt.focussearch.points = 300L)
control = setMBOControlMultiFid(control = control, 
                                param = "dw.perc", 
                                lvls = c(0.1, 0.3, 0.5, 1),
                                costs = function(cur, last) (last / cur)^1.3)
surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim = TRUE, jitter = TRUE)
result = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control, show.info = TRUE)

x = seq(from=par.set$pars$x$lower, to=par.set$pars$x$upper, length.out=200)
df = expand.grid(x=x, dw.perc = control$multifid.lvls)
df$value = apply(df, 1, function(z) objfun(list(z["x"], dw.perc = z["dw.perc"])))
df$variable = "response"
print(df[which.min(df$value),])
add.g = list(geom_line(data = df, alpha = 0.5, lty = 2),
             scale_color_gradient(low = "green", high = "blue"))

pdf("multifid_steps_test_fail.pdf", width=10, height=12)
for (i in seq_along(result$plot.data)) {
  plot = plotMultiFidStep(result$plot.data[[i]], title = sprintf("Step %i", i), add.g = add.g)
  print(plot)
}
dev.off()
as.data.frame(result$opt.path)
result$y.hat



### DEBUG ####

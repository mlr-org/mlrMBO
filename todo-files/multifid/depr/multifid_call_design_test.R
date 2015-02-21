#mlrMBO workflow
#library("mlrMBO")
library("lhs")
library(devtools)
library("BBmisc")
library("ParamHelpers")

load_all(".")
options(warn = 2)

set.seed(1)

source("todo-files/test_functions.R")

par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 10),
  makeNumericParam("dw.perc", lower=0, upper=1)
)

objfun = makeAddFunction(fun=bakeFunction(sasena), addfun=uppMove, fac = 1)
objfun(x=list(x=1, dw.perc=0.1), lvl.par="dw.perc")
objfun2 = function(x, lvl.par) {x$dw.perc = head(e.lvl) objfun(x)}
  
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
                                lvls = c(0.1, 0.3, 1),
                                costs = function(cur, last) (last / cur)^0.5,
                                cor.grid.points = 100)
surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim = TRUE, jitter = TRUE)
result = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control, show.info = TRUE)

x = seq(from=par.set$pars$x$lower, to=par.set$pars$x$upper, length.out=200)
df = expand.grid(x=x, dw.perc = control$multifid.lvls)
df$value = apply(df, 1, function(z) objfun(list(z["x"], dw.perc = z["dw.perc"])))
df$variable = "response"
print(df[which.min(df$value),])
add.g = list(geom_line(data = df, alpha = 0.5, lty = 2),
             scale_color_gradient(low = "green", high = "blue"))

pdf("multifid_steps_test.pdf", width=10, height=12)
for (i in seq_along(result$plot.data)) {
  plot = plotMultiFidStep(result$plot.data[[i]], title = sprintf("Step %i", i), add.g = add.g, subset.variable = c("response", "crit", "ei", "se", "alpha1", "alpha2"))
  print(plot)
}
dev.off()
as.data.frame(result$opt.path)
result$y.hat



### DEBUG ####

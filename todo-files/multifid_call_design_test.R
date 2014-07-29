#mlrMBO workflow
#library("mlrMBO")
library("lhs")
library(devtools)
library("BBmisc")
library("ParamHelpers")
library("checkmate")
load_all(".")
options(warn = 2)

source("todo-files/test_functions.R")

par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 10),
  makeNumericParam("dw.perc", lower=0, upper=1)
)

objfun = makeAddFunction(fun=bakeFunction(sasena), addfun=uppMove)
  
control = makeMBOControl(
  init.design.points = 20L, #distributed over the different levels, seems not to work for <5 each
  init.design.fun = maximinLHS,
  iters = 10,
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
                                lvls = c(0.1, 0.3, 0.5, 1))
surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim = TRUE, jitter = TRUE)
result = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control, show.info = TRUE)
pdf("multifid_steps_test.pdf", width=10, height=12)
for (i in seq_along(result$plot.data)) {
  plot = genGgplot(result$plot.data[[i]], title = sprintf("Step %i", i))
  print(plot)
}
dev.off()
as.data.frame(result$opt.path)
result$y.hat

x = seq(from=par.set$pars$x$lower, to=par.set$pars$x$upper, length.out=200)
df = expand.grid(x=x, dw.perc = control$multifid.lvls)
df$val = apply(df, 1, function(z) sp(list(z["x"], dw.perc = z["dw.perc"])))
g = ggplot(df, aes(x=x, y=val, color=dw.perc, group=dw.perc))
g + geom_line()

### DEBUG ####

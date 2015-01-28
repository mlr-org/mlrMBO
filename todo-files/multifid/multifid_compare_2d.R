source("todo-files/test_functions.R")
source("todo-files/multifid/benchmark/data_benchmark.R")
source("todo-files/multifid/benchmark/generic.R")
source("todo-files/multifid/benchmark/helpers.R")
source("todo-files/multifid/benchmark/plots.R")

e.seed = 10243
options(warn=2)

e.control = makeMBOControl(
  init.design.points = 50L, #distributed over the different levels, seems not to work for <5 each
  init.design.fun = maximinLHS,
  iters = 10L,
  on.learner.error = "stop",
  show.learner.output = FALSE,
)
e.control = setMBOControlInfill(
  control = e.control, 
  opt = "focussearch", 
  opt.restarts = 1L, 
  opt.focussearch.maxit = 1L, 
  opt.focussearch.points = 100L,
  filter.proposed.points = TRUE,
  filter.proposed.points.tol = 0.001
)

e.string = paste0("km_",format(Sys.time(), "%Y_%m_%d-%H%M"))

# Define learner and parameter
e.lrn = makeLearner("classif.LiblineaRBinary", type = 1)
e.par.set = makeParamSet(
  makeNumericParam("cost", lower = -15, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("epsilon", lower = -20, upper = 2, trafo = function(x) 2^x)
)
e.lvl = c(0.1, 0.2) #, 0.5, 1)
#e.lvl = c(0.1, 0.3, 1)

surrogat.model = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)

#dat.w7a = libsvm.read("../data/w7a")

#e.task = makeClassifTask(id = "w7a", data = dat.w7a, target = "Y")
#e.rin = makeResampleInstance("Holdout", task = e.task)
#dat.res = dataBenchmark(e.name = "w7a", e.task = e.task, e.rin = e.rin, e.lrn = e.lrn, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, surrogat.model = surrogat.model, e.string = e.string, grid.all = TRUE)

e.par.set = makeParamSet(
  makeNumericParam("x1", lower = 0, upper = 15),
  makeNumericParam("x2", lower = 0, upper = 15)
)

hartmans = list(hartman10 = 1, hartman07 = 0.7, hartman05 = 0.5, hartman02 = 0.2)
#hartmans = list(hartman10 = 1)
hartmans.res = lapply(names(hartmans), function(sn) {
  #objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman2d, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
  objfun = makeMBOMultifidFunction(f = addDistortion(hartman2d, yupp, fac = hartmans[[sn]]), lvls = e.lvl)
  generalBenchmark(e.name = sn, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, control = e.control, grid.all = TRUE, e.string = e.string, high.res = TRUE)
})

save.image(paste0("plots/", e.string, "/multifid_compare.RData"))




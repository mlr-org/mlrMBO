source("todo-files/multifid/benchmark/data_benchmark.R")
source("todo-files/multifid/benchmark/generic.R")
source("todo-files/multifid/benchmark/helpers.R")
source("todo-files/multifid/benchmark/plots.R")
source("todo-files/test_functions.R")

e.seed = 10243
options(warn=2)

e.control = makeMBOControl(
  init.design.points = 20L, #distributed over the different levels, seems not to work for <5 each
  init.design.fun = maximinLHS,
  iters = 20L,
  on.learner.error = "stop",
  show.learner.output = FALSE,
)
e.control = setMBOControlInfill(
  control = e.control, 
  opt = "focussearch",
  opt.focussearch.points = 100L,
  opt.restarts = 2L
)

e.string = paste0("km_",format(Sys.time(), "%Y_%m_%d-%H%M"))

e.lvl = c(0.1, 0.2, 0.5, 1)

surrogat.model = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)

e.par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 10)
)

sasenas = list(sasena10 = 1, sasena07 = 0.7, sasena05 = 0.5, sasena02 = 0.2)
#sasenas = list(sasena02 = 0.5)
sasens.res = lapply(names(sasenas), function(sn) {
  objfun = makeMBOMultifidFunction(f = addDistortion(sasena, g = yshift, fac = sasenas[[sn]]), lvls = e.lvl)
  generalBenchmark(e.name = sn, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = TRUE, surrogat.model = surrogat.model, e.string = e.string, high.res = TRUE, multifid.costs = e.lvl)
})

e.par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 15)
)

hartmans = list(hartman10 = 1, hartman07 = 0.7, hartman05 = 0.5, hartman02 = 0.2)
#hartmans = list(hartman10 = 1)
hartmans.res = lapply(names(hartmans), function(sn) {
  objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
  generalBenchmark(e.name = sn, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = TRUE, e.string = e.string, high.res = TRUE, multifid.costs = e.lvl)
})

##
#### DATA BENCHMARK #####

# Define learner and parameter
e.lrn = makeLearner("classif.LiblineaRBinary", type = 1)
e.par.set = makeParamSet(
  makeNumericParam("cost", lower = -15, upper = 5, trafo = function(x) 2^x)
)

dat.w7a = libsvm.read("../data/w7a")
dat.w8a = libsvm.read("../data/w8a")

e.task = makeClassifTask(id = "w7a", data = dat.w7a, target = "Y")
e.rin = makeResampleInstance("Holdout", task = e.task)
dat.w7a.res = dataBenchmark(e.name = "w7a", e.task = e.task, e.rin = e.rin, e.lrn = e.lrn, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, surrogat.model = surrogat.model, e.string = e.string, grid.all = TRUE, high.res = TRUE)

e.task = makeClassifTask(id = "w8a", data = dat.w8a, target = "Y")
e.rin = makeResampleInstance("Holdout", task = e.task)
dat.w8a.res = dataBenchmark(e.name = "w8a", e.task = e.task, e.rin = e.rin, e.lrn = e.lrn, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, surrogat.model = surrogat.model, e.string = e.string, grid.all = TRUE, high.res = TRUE)

save.image(paste0("plots/", e.string, "/multifid_compare.RData"))


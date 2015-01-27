source("todo-files/multifid/multifid_benchmark.R")
source("todo-files/multifid/multifid_benchmark_generic.R")
source("todo-files/test_functions.R")

e.seed = 10243
options(warn=2)

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

dat.w7a = libsvm.read("../data/w7a")

e.task = makeClassifTask(id = "w7a", data = dat.w7a, target = "Y")
e.rin = makeResampleInstance("Holdout", task = e.task)
# dat.res = dataBenchmark(e.name = "w7a", e.task = e.task, e.rin = e.rin, e.lrn = e.lrn, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, surrogat.model = surrogat.model, e.string = e.string, grid.all = TRUE)

e.par.set = makeParamSet(
  makeNumericParam("x1", lower = 0, upper = 15),
  makeNumericParam("x2", lower = 0, upper = 15)
)

#hartmans = list(hartman10 = 1, hartman07 = 0.7, hartman05 = 0.5, hartman02 = 0.2)
hartmans = list(hartman10 = 1)
hartmans.res = lapply(names(hartmans), function(sn) {
  #objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman2d, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
  objfun = makeMBOMultifidFunction(f = addDistortion(hartman2d, yupp, fac = hartmans[[sn]]), lvls = e.lvl)
  generalBenchmark(e.name = sn, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = TRUE, e.string = e.string)
})

save.image(paste0("plots/", e.string, "/multifid_compare.RData"))




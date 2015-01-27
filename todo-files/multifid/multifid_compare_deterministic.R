source("todo-files/multifid/multifid_benchmark.R")
source("todo-files/multifid/multifid_benchmark_generic.R")
source("todo-files/test_functions.R")

e.seed = 10243

e.string = paste0("km_",format(Sys.time(), "%Y_%m_%d-%H%M"))

# Define learner and parameter
e.lrn = makeLearner("classif.LiblineaRBinary", type = 1)

e.lvl = c(0.1, 0.2, 0.5, 1)
#e.lvl = c(0.1, 0.3, 1)

surrogat.model = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)

e.par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 10)
)

sasenas = list(sasena10 = 1, sasena07 = 0.7, sasena05 = 0.5, sasena02 = 0.2)
#sasenas = list(sasena02 = 0.5)
sasens.res = lapply(names(sasenas), function(sn) {
  objfun = makeMBOMultifidFunction(f = addDistortion(sasena, g = yshift, fac = sasenas[[sn]]), lvls = e.lvl)
  generalBenchmark(e.name = sn, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = TRUE, surrogat.model = surrogat.model, e.string = e.string)
})

e.par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 15)
)

hartmans = list(hartman10 = 1, hartman07 = 0.7, hartman05 = 0.5, hartman02 = 0.2)
hartmans.res = lapply(names(hartmans), function(sn) {
  objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
  generalBenchmark(e.name = sn, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = TRUE, e.string = e.string)
})

save.image(paste0("plots/", e.string, "/multifid_compare.RData"))


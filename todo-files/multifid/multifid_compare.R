source("todo-files/multifid/multifid_benchmark.R")
source("todo-files/multifid/multifid_benchmark_generic.R")
source("todo-files/test_functions.R")

e.seed = 44137

# Define learner and parameter
e.lrn = makeLearner("classif.LiblineaRBinary", type = 1)
e.par.set = makeParamSet(
  makeNumericParam("cost", lower = -15, upper = 5, trafo = function(x) 2^x)
)
#e.lvl = c(0.1, 0.3, 0.5, 1)
e.lvl = c(0.1, 0.3, 1)

surrogat.model = makeLearner("regr.randomForest", ntree = 20L)

#openML.ids = c(spambase = 273, bng = 2328)
#openML.res = lapply(openML.ids, openMLBenchmark, e.seed = e.seed, e.lrn = e.lrn, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = FALSE)
library("foreign")
bng = read.arff("../data/puma32H.arff")
bng = BBmisc::convertDfCols(df = bng)
e.task = makeClassifTask(id = "bng", data = bng, target = "binaryClass")
e.rin = makeResampleInstance("Holdout", task = e.task)

dat.res = dataBenchmark(e.name = "BNG", e.task = e.task, e.rin = e.rin, e.lrn = e.lrn, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, surrogat.model = surrogat.model)

e.par.set = makeParamSet(
  makeNumericParam("cost", lower = 0, upper = 10)
)
#sasenas = list(sasena10 = 1, sasena07 = 0.7, sasena05 = 0.5, sasena02 = 0.2)
sasenas = list(sasena02 = 0.5)
sasens.res = lapply(names(sasenas), function(sn) {
  objfun = makeAddFunction(fun=bakeFunction(sasena), addfun=uppMove, fac = sasenas[[sn]])
  generalBenchmark(e.name = sn, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = TRUE, surrogat.model = surrogat.model)
})

# hartmans = list(hartman10 = 1, hartman07 = 0.7, hartman05 = 0.5, hartman02 = 0.2)
# hartmans.res = lapply(names(hartmans), function(sn) {
#   objfun = makeShiftFunction(makeAddFunction(fun=bakeFunction(hartman), addfun=uppMove, fac = 0.2), direction = hartmans[[sn]])
#   generalBenchmark(e.name = sn, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = FALSE)
# })

save.image("multifid_compare.RData")


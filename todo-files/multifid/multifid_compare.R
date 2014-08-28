source("todo-files/multifid/multifid_benchmark.R")
source("todo-files/test_functions.R")

e.seed = 44137

# Define learner and parameter
e.lrn = makeLearner("classif.LiblineaRBinary", type = 1)
e.par.set = makeParamSet(
  makeNumericParam("cost", lower = -15, upper = 5, trafo = function(x) 2^x)
)
e.lvl = c(0.1, 0.3, 1)

openML.ids = c(spambase = 273, bng = 2328, creditg = 2328)
openML.res = lapply(openML.ids, openMLBenchmark, e.seed = e.seed, e.lrn = e.lrn, e.par.set = e.par.set, e.lvl = e.lvl, alpha2fix = TRUE)

e.par.set = makeParamSet(
  makeNumericParam("cost", lower = 0, upper = 10)
)
sasenas = list(sasena10 = 1, sasena07 = 0.7, sasena05 = 0.5, sasena02 = 0.2)
sasens.res = lapply(names(sasenas), function(sn) {
  objfun = makeAddFunction(fun=bakeFunction(sasena), addfun=uppMove, fac = sasenas[[sn]])
  generalBenchmark(e.name = sn, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, alpha2fix = TRUE)
})


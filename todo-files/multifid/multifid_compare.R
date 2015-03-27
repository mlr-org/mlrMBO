library(checkmate)
library(mlr)
library(mlrMBO)
library(BatchExperiments)
library(ggplot2)
library(gridExtra)
library(foreign)
source("todo-files/multifid/benchmark/data_benchmark.R")
source("todo-files/multifid/benchmark/generic.R")
source("todo-files/multifid/benchmark/helpers.R")
source("todo-files/multifid/benchmark/plots.R")
source("todo-files/multifid/benchmark/giveMe.R")
source("todo-files/test_functions.R")

e.seed = 5566
options(warn = 0)

e.control = giveMeMBOControl(budget = 36L, exec.time.budget = NULL, time.budget = NULL, noisy = FALSE)
e.control$filter.proposed.points.tol = 0.01
e.control2 = e.control
e.control2$filter.proposed.points.tol = 0.0001
e.control3 = e.control
e.control3$init.design.points = 30L
e.control3$filter.proposed.points.tol = 0.0005

if (!exists("e.string"))
  e.string = paste0("analyse",format(Sys.time(), "%Y_%m_%d-%H%M/1d"))
if (!exists("e.string2"))
  e.string2 = paste0("analyse",format(Sys.time(), "%Y_%m_%d-%H%M/2d"))
if (!exists("e.string3"))
  e.string3 = paste0("analyse",format(Sys.time(), "%Y_%m_%d-%H%M/3d"))

e.lvl = giveMeLvl()

surrogat.model = giveMeSurrogatLearner("deterministic")

e.par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 10)
)

sasenas = list(sasena10 = 1, sasena07 = 0.7, sasena05 = 0.5, sasena02 = 0.2)
#sasenas = list(sasena02 = 0.5)
if (!exists("sasens.res")) {
  sasens.res = lapply(names(sasenas), function(sn) {
    objfun = makeMBOMultifidFunction(f = addDistortion(sasena, g = yshift, fac = sasenas[[sn]]), lvls = e.lvl)
    generalBenchmark(e.name = sn, objfun = objfun, control = e.control, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = TRUE, surrogat.model = surrogat.model, e.string = e.string, high.res = TRUE, multifid.costs = e.lvl)
  })
  save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))
} else {
  messagef("skip sasens.res")
}

e.par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 15)
)
e.par.set2 = makeParamSet(
  makeNumericParam("x1", lower = 0, upper = 15),
  makeNumericParam("x2", lower = 0, upper = 15)
)
e.par.set3 = makeParamSet(
  makeNumericParam("x1", lower = 0, upper = 15),
  makeNumericParam("x2", lower = 0, upper = 15),
  makeNumericParam("x3", lower = 0, upper = 15)
)

hartmans = list(hartman10 = 1, hartman07 = 0.7, hartman05 = 0.5, hartman02 = 0.2)
#hartmans = list(hartman10 = 1)

if (!exists("hartmans.res"))
  hartmans.res = list()
for(sn in names(hartmans)) {
  if (is.null(hartmans.res[[sn]])) {
    objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
    hartmans.res[[sn]] = generalBenchmark(e.name = sn, objfun = objfun, control = e.control, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = TRUE, surrogat.model = surrogat.model, e.string = e.string, high.res = TRUE, multifid.costs = e.lvl)
    save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))
  } else {
    messagef("skip %s", sn)
  }
}

if (!exists("hartmans.res2"))
  hartmans.res2 = list()
for(sn in names(hartmans)) {
  if (is.null(hartmans.res2[[sn]])) {
    objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman2d, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
    hartmans.res2[[sn]] = generalBenchmark(e.name = sn, objfun = objfun, control = e.control2, e.seed = e.seed, e.par.set = e.par.set2, e.lvl = e.lvl, grid.all = TRUE, surrogat.model = surrogat.model, e.string = e.string2, high.res = TRUE, multifid.costs = e.lvl)
    save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))
  } else {
    messagef("skip 2d %s", sn)
  }
}

if (!exists("hartmans.res3"))
  hartmans.res3 = list()
for(sn in names(hartmans)) {
  if (is.null(hartmans.res3[[sn]])) {
    objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman3d, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
    hartmans.res3[[sn]] = generalBenchmark(e.name = sn, objfun = objfun, control = e.control3, e.seed = e.seed, e.par.set = e.par.set3, e.lvl = e.lvl, grid.all = TRUE, surrogat.model = surrogat.model, e.string = e.string3, high.res = FALSE, multifid.costs = e.lvl)
    save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))
  } else {
    messagef("skip 3d %s", sn)
  }
}

##
#### DATA BENCHMARK #####

# Define learner and parameter
e.lrn = giveMeLearners("svm")[[1]]
e.par.set3 = giveMeParamSets(list(e.lrn))[[1]]
e.par.set = dropParams(e.par.set3, c("cost", "tolerance"))
e.par.set2 = dropParams(e.par.set3, c("tolerance"))
e.task = giveMeTasks(x = c("nursery"))[[1]]
#e.control$filter.proposed.points.tol = 0.005
e.control$noisy = TRUE

surrogat.model = giveMeSurrogatLearner()

e.rin = makeResampleInstance("Holdout", task = e.task)

if (!exists("dat.nursery.res")) {
  dat.nursery.res = dataBenchmark(e.name = "nursery", e.task = e.task, e.rin = e.rin, e.lrn = e.lrn, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, surrogat.model = surrogat.model, control = e.control, e.string = e.string, grid.all = TRUE, high.res = TRUE)
} else {
  messagef("skip dat.nursery.res 1d")
}
save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))

if (!exists("dat.nursery2.res")) {
  dat.nursery2.res = dataBenchmark(e.name = "nursery2", e.task = e.task, e.rin = e.rin, e.lrn = e.lrn, e.seed = e.seed, e.par.set = e.par.set2, e.lvl = e.lvl, surrogat.model = surrogat.model, control = e.control, e.string = e.string2, grid.all = TRUE, high.res = TRUE)
} else {
  messagef("skip dat.nursery2.res 2d")
}
save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))


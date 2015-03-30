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

e.seed = 1511
options(warn = 0)
allOrAny = all #whcih function to choose to overwrite
e.control = giveMeMBOControl(budget = 36L, exec.time.budget = NULL, time.budget = NULL, noisy = FALSE)
e.control$filter.proposed.points.tol = 0.01
e.control2 = e.control
e.control2$filter.proposed.points.tol = 0.0001
e.control3 = e.control
e.control3$init.design.points = 30L
e.control3$filter.proposed.points.tol = 0.0005

if (!exists("e.string"))
  e.string = paste0("analyse_rep",format(Sys.time(), "%Y_%m_%d-%H%M/1d"))
if (!exists("e.string2"))
  e.string2 = paste0("analyse_rep",format(Sys.time(), "%Y_%m_%d-%H%M/2d"))
if (!exists("e.string3"))
  e.string3 = paste0("analyse_rep",format(Sys.time(), "%Y_%m_%d-%H%M/3d"))

e.lvl = giveMeLvl()

surrogat.model = giveMeSurrogatLearner("deterministic")
reps = 30L

e.par.set = makeParamSet(
  makeNumericParam("x", lower = 0, upper = 10)
)

sasenas = list(sasena10 = 1, sasena07 = 0.7, sasena05 = 0.5, sasena02 = 0.2)
#sasenas = list(sasena02 = 0.5)
if (!exists("sasenas.res"))
  sasenas.res = list()
for(sn in names(sasenas)) {
  if (is.null(sasenas.res[[sn]]) || allOrAny(sapply(sasenas.res[[sn]], is.null))) {
    objfun = makeMBOMultifidFunction(f = addDistortion(sasena, g = yshift, fac = sasenas[[sn]]), lvls = e.lvl)
    sasenas.res[[sn]] = lapply(seq_len(reps), function(i) {
      try(generalBenchmark(e.name = sn, objfun = objfun, control = e.control, e.seed = e.seed + i, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = FALSE, surrogat.model = surrogat.model, e.string = e.string, high.res = FALSE, multifid.costs = e.lvl, only.table = TRUE)$bench.table)
    })
    save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))
  } else {
    messagef("skip %s", sn)
  }
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
  if (is.null(hartmans.res[[sn]]) || allOrAny(sapply(hartmans.res[[sn]], is.null))) {
    objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
    hartmans.res[[sn]] = lapply(seq_len(reps), function(i) {
      try(generalBenchmark(e.name = sn, objfun = objfun, control = e.control, e.seed = e.seed + i, e.par.set = e.par.set, e.lvl = e.lvl, grid.all = FALSE, surrogat.model = surrogat.model, e.string = e.string, high.res = FALSE, multifid.costs = e.lvl, only.table = TRUE)$bench.table)
    })
    save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))
  } else {
    messagef("skip %s", sn)
  }
}


if (!exists("hartmans.res2"))
  hartmans.res2 = list()
for(sn in names(hartmans)) {
  if (is.null(hartmans.res2[[sn]]) || allOrAny(sapply(hartmans.res2[[sn]], is.null))) {
    objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman2d, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
    hartmans.res2[[sn]] = lapply(seq_len(reps), function(i) {
      try(generalBenchmark(e.name = sn, objfun = objfun, control = e.control2, e.seed = e.seed + i, e.par.set = e.par.set2, e.lvl = e.lvl, grid.all = FALSE, surrogat.model = surrogat.model, e.string = e.string2, high.res = FALSE, multifid.costs = e.lvl, only.table = TRUE)$bench.table)
    })
    save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))
  } else {
    messagef("skip %s", sn)
  }
}

if (!exists("hartmans.res3"))
  hartmans.res3 = list()
for(sn in names(hartmans)) {
  if (is.null(hartmans.res3[[sn]]) || allOrAny(sapply(hartmans.res3[[sn]], is.null))) {
    objfun = makeMBOMultifidFunction(f = distortX(addDistortion(hartman3d, yupp, fac = hartmans[[sn]]), xshift, direction = hartmans[[sn]]), lvls = e.lvl)
    hartmans.res3[[sn]] = lapply(seq_len(reps), function(i) {
      try(generalBenchmark(e.name = sn, objfun = objfun, control = e.control3, e.seed = e.seed + i, e.par.set = e.par.set3, e.lvl = e.lvl, grid.all = FALSE, surrogat.model = surrogat.model, e.string = e.string3, high.res = FALSE, multifid.costs = e.lvl, only.table = TRUE)$bench.table)
    })
    save.image(paste0("../plots/", e.string, "/multifid_compare.RData"))
  } else {
    messagef("skip %s", sn)
  }
}


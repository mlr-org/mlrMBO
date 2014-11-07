# 0. Load packages and seed
library("devtools")
library("BBmisc")
library("mlr")
library("OpenML")
library("ggplot2")
library("reshape2")
library("plyr")
load_all()

openMLBenchmark = function (task.id, e.seed, e.lrn, e.par.set, e.lvl, ...) {
  task.openML = downloadOpenMLTask(id = task.id)
  e.name = paste0(task.openML$data.desc$name,"_",task.openML$id)
  openML.as.mlr = toMlr(task.openML)
  e.task = openML.as.mlr$mlr.task
  e.rin = openML.as.mlr$mlr.rin
  dataBenchmark(e.name, e.task, e.rin, e.seed, e.lrn, e.par.set, e.lvl, ...)
}

dataBenchmark = function (e.name, e.task, e.rin, e.seed, e.lrn, e.par.set, e.lvl, control = NULL, surrogat.model = NULL, grid.all = FALSE) {
  lrn = makeDownsampleWrapper(learner = e.lrn, dw.stratify = TRUE, dw.perc = 1)
  
  makeObjFun = function(lrn, task, rsm) {
    force(lrn)
    force(task)
    force(rsm)
    #we could also implement a fixed holdout for each level here if provided par.set
    function(x) {
      # produce train test split manually. then downsample training further.
      lrn.local = setHyperPars(lrn, par.vals=x)
      y = resample(lrn.local, task, rsm, show.info=FALSE)$aggr[[1L]]
      return(y)
    }
  }
  
  objfun = makeObjFun(lrn, e.task, e.rin)
  
  generalBenchmark(e.name = e.name, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, surrogat.model = surrogat.model, control = control, grid.all = grid.all)
}
  



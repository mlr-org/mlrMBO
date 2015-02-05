openMLBenchmark = function (task.id, e.seed, e.lrn, e.par.set, e.lvl, ...) {
  task.openML = downloadOpenMLTask(id = task.id)
  e.name = paste0(task.openML$data.desc$name,"_",task.openML$id)
  openML.as.mlr = toMlr(task.openML)
  e.task = openML.as.mlr$mlr.task
  e.rin = openML.as.mlr$mlr.rin
  dataBenchmark(e.name, e.task, e.rin, e.seed, e.lrn, e.par.set, e.lvl, ...)
}

dataBenchmark = function (e.name, e.task, e.rin, e.seed, e.lrn, e.par.set, e.lvl, control = NULL, surrogat.model = NULL, grid.all = FALSE, ...) {
  lrn = makeDownsampleWrapper(learner = e.lrn, dw.stratify = TRUE, dw.perc = 1)
  
  makeObjFun = function(lrn, task, rsm, lvl) {
    force(lrn)
    force(task)
    force(rsm)
    force(lvl)
    #we could also implement a fixed holdout for each level here if provided par.set
    function(x) {
      # produce train test split manually. then downsample training further.
#       cat("x", "\n")
#       print(x)
#       cat("lvl", "\n")
#       print(lvl)
      x$dw.perc = lvl[x$.multifid.lvl]
      x = dropNamed(x, ".multifid.lvl")
#       cat("x used", "\n")
#       print(x)
      lrn.local = setHyperPars(lrn, par.vals=x)
      y = resample(lrn.local, task, rsm, show.info=FALSE)$aggr[[1L]]
      return(y)
    }
  }
  
  objfun = makeObjFun(lrn, e.task, e.rin, e.lvl)
  
  generalBenchmark(e.name = e.name, objfun = objfun, e.seed = e.seed, e.par.set = e.par.set, e.lvl = e.lvl, surrogat.model = surrogat.model, control = control, grid.all = grid.all, ...)
}
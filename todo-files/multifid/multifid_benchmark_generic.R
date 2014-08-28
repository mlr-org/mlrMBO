# 0. Load packages and seed
library("devtools")
library("BBmisc")
library("mlr")
library("OpenML")
library("ggplot2")
library("reshape2")
library("plyr")

load_all()

generalBenchmark = function(e.name, objfun, e.seed, e.par.set, e.lvl, surrogat.model = NULL, control = NULL, alpha2fix = FALSE) {
  if (is.null(control)) {
    # 4. common parameters
    control.common = makeMBOControl(
      init.design.points = 20L, #distributed over the different levels, seems not to work for <5 each
      init.design.fun = maximinLHS,
      iters = 20,
      on.learner.error = "stop",
      show.learner.output = FALSE,
    )
    control.common = setMBOControlInfill(
      control = control.common, 
      opt = "focussearch", 
      opt.restarts = 1L, 
      opt.focussearch.maxit = 1L, 
      opt.focussearch.points = 300L
    )
  } else {
    control.common = control
  }
  control.common$multifid.alpha2fix = alpha2fix
  
  if (is.null(surrogat.model)) {
    surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim = TRUE, jitter = TRUE)
  }
  
  
  # 4.1 Wrapper + Obj Func
  lrn.par.set = makeParamSet(
    makeNumericParam("dw.perc", lower=0, upper=1)
  )
  par.set = c(e.par.set, lrn.par.set)
  
  # 5.0 Initiate result list
  mbo.res = list()
  
  # 5. mbo Full experiment
  set.seed(e.seed)
  objfun.expansive = function(x, ...) {
    dots = list(...)
    if(!is.null(dots$lvl.par))
      lvl.par = lvl.par
    else
      lvl.par = "dw.perc"
    x[[lvl.par]] = tail(e.lvl,1)
    objfun(x, ...)
  }
  mbo1.time = system.time( {mbo1 = mbo(fun = objfun.expansive, e.par.set, learner = surrogat.model, control = control.common, show.info = TRUE) })
  sum(mbo1$opt.path$env$exec.time)
  mbo1$system.time = mbo1.time
  mbo.res$mbo_expansive = mbo1
  
  # 6. mbo cheapest experiment
  set.seed(e.seed)
  objfun.cheap = function(x, ...) {
    dots = list(...)
    if(!is.null(dots$lvl.par))
      lvl.par = lvl.par
    else
      lvl.par = "dw.perc"
    x[[lvl.par]] = head(e.lvl,1)
    objfun(x, ...)
  }
  mbo2.time = system.time( {mbo2 = mbo(fun = objfun.cheap, e.par.set, learner = surrogat.model, control = control.common, show.info = TRUE) })
  sum(mbo2$opt.path$env$exec.time)
  mbo2$system.time = mbo2.time
  mbo.res$mbo_cheap = mbo2
  
  # 7. multifid
  set.seed(e.seed)
  control.multifid = control.common
  control.multifid$infill.crit = "multiFid"
  control.multifid = setMBOControlMultiFid(
    control = control.multifid, 
    param = "dw.perc", 
    lvls = e.lvl,
    costs = function(cur, last) (last / cur)^1.5
  )
  mbo3.time = system.time({mbo3 = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control.multifid, show.info = TRUE)})
  mbo3$system.time = mbo3.time
  df = as.data.frame(mbo3$opt.path)
  df = df[df$dob > 0,]
  mbo3$perf.steps = table(df$dw.perc, cut(df$dob,3))
  
  mbo.res$multifid = mbo3
  
  # 8. grid Search
  set.seed(e.seed)
  #source("../ParamHelpers/todo-files/discretizeParam.R")
  #par.set.disc = lapply(e.par.set$pars, discretizeParam, length.out = control.common$init.design.points + control.common$iters, trafo = TRUE)
  #par.set.disc = makeParamSet(params = par.set.disc)
  par.set.lower = dropParams(par.set, "dw.perc")
  grid.design = generateGridDesign(par.set = par.set.lower, resolution = control.common$init.design.points + control.common$iters)
  control.grid = makeMBOControl(iters = 1)
  control.grid$iters = 0
  #grid.design[,"dw.perc"] = tail(e.lvl, 1)
  #mbo4.time = system.time({mbo4 = tuneParams(e.lrn, task = e.task, resampling = e.rin, par.set = par.set.disc, control = ctrl.grid)})
  mbo4.time = system.time({mbo4 = mbo(fun = objfun.expansive, par.set = par.set.lower, design = grid.design, learner = surrogat.model, control = control.grid, show.info = TRUE)})
  mbo4$system.time = mbo4.time
  mbo.res$grid = mbo4
  
  
  # 9. Visualisation
  # preproc grid search
  df.grid = as.data.frame(mbo4$opt.path)
  
  # 9.1 mbo Full + mbo Cheam + grid
  df.grid.1 = rename(df.grid, c("y"="y"))
  df.grid.1$level = "expensive"
  op1 = as.data.frame(mbo1$opt.path)
  op2 = as.data.frame(mbo2$opt.path)
  df = rbind(cbind(op1, level = "expensive"), cbind(op2, level = "cheapest"))
  g = ggplot(df, aes_string(x = getParamIds(e.par.set), y = "y", shape = "level", color = "dob"))
  g = g + geom_point(size = 4, alpha = 0.6)
  g = g + geom_line(data = df.grid.1, alpha = 0.5, lty = 2, color = "black")
  g
  ggsave(paste0(e.name,"_mbo1and2.pdf"), width = 8, height = 5)
  
  # 9.2 multiFid + grid
  df.grid.2 = rename(df.grid, c("y"="value"))
  df.grid.2$variable = "response"
  df.grid.2$dw.perc = 1
  add.g = list(
    geom_line(data = df.grid.2, alpha = 0.5, lty = 2),
    scale_color_gradient2(low = "green", high = "red", mid="yellow", midpoint=median(e.lvl))
  )
  versions = list(
    all = NULL,
    crit = c("response","crit"),
    crit_ei = c("response","crit", "ei"),
    crit_alpha2 = c("response","crit", "alpha2")
  )
  lapply(names(versions), function(v.name) {
    subs = versions[[v.name]]
    p.height = ifelse(is.null(subs), 12, length(subs) * 3)
    pdf(paste0(e.name,"_multifid_steps_",v.name,".pdf"), width = 10, height = p.height)
    for (i in seq_along(mbo3$plot.data)) {
      plot = genGgplot(mbo3$plot.data[[i]], title = sprintf("Step %i", i), add.g = add.g, subset.variable = subs)
      print(plot)
    }
    dev.off()
    NULL
  })
  
  # 9.2.5 Multifid Steps As plot (and table)
  df = as.data.frame(mbo.res$multifid$opt.path)
  df = df[df$dob>0,]
  g = ggplot(df, aes(y = dw.perc, x = dob))
  g = g + geom_line() + geom_point(aes(size = y)) 
  ggsave(filename = paste0(e.name, "_multifid_steps.pdf"), plot = g, width = 7, height = 5)
  
  # 9.3 Compare different methods end result
  df = data.frame(
    method = names(mbo.res),
    x = unlist(extractSubList(mbo.res,"x")),
    y = extractSubList(mbo.res,"y"),
    time = extractSubList(mbo.res,list("system.time",1))
  )
  g = ggplot()
  g = g + geom_line(data = df.grid.2, alpha = 0.5, mapping = aes(x = cost, y = value))
  g = g + geom_point(data = df, mapping = aes(x = x, y = y, color = method))
  g = g + geom_text(data = df, mapping = aes(x = x, y = y, label = time, color = method, angle = 70, hjust = -0.2))
  ggsave(filename = paste0(e.name, "_res_compare.pdf"), plot = g, width = 10, height = 5)
  
  mbo.res  
}

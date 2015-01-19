# 0. Load packages and seed
library("devtools")
library("BBmisc")
library("mlr")
library("ggplot2")
library("reshape2")
library("plyr")

load_all()

generalBenchmark = function(e.name, objfun, e.seed, e.par.set, e.lvl, surrogat.model = NULL, control = NULL, grid.all = TRUE, e.string = NULL) {
  # store plots in subdirectory
  if(!is.null(e.string)) {
    dir.create(paste0("plots/", e.string))
    e.name = paste0(e.string, "/", e.name)
  }
  # initialize control if not sets
  if (is.null(control)) {
    # 4. common parameters
    control.common = makeMBOControl(
      init.design.points = 20L, #distributed over the different levels, seems not to work for <5 each
      init.design.fun = maximinLHS,
      iters = 20L,
      on.learner.error = "stop",
      show.learner.output = FALSE,
    )
    control.common = setMBOControlInfill(
      control = control.common, 
      opt = "focussearch", 
      opt.restarts = 1L, 
      opt.focussearch.maxit = 1L, 
      opt.focussearch.points = 100L,
      filter.proposed.points = TRUE,
      filter.proposed.points.tol = 0.001
    )
  } else {
    control.common = control
  }
  control.multifid = control.common
  control.multifid$infill.crit = "multiFid"
  control.multifid = setMBOControlMultiFid(
    control = control.multifid, 
    param = "dw.perc", 
    lvls = e.lvl,
    cor.grid.points = 40L,
    costs = function(cur, last) (last / cur)^1.8
  )
  
  if (is.null(surrogat.model)) {
    #surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim = TRUE, jitter = TRUE)
    surrogat.model = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)
  }
  
  # generate different objfuns in a list
  objfuns = lapply(seq_along(e.lvl), function(lvl) {
    force(lvl)
    function(x) {
      x$.multifid.lvl = lvl
      objfun(x)
    }
  })
  
  
  # 4.1 Wrapper + Obj Func
  par.set = e.par.set
  
  # 5.0 Initiate result list
  mbo.res = list()
  
  # 5. mbo Full experiment
  set.seed(e.seed)
  catf("5. mbo Full Experiment")
  mbo1.time = system.time( {mbo1 = mbo(fun = getLast(objfuns), e.par.set, learner = surrogat.model, control = control.common, show.info = TRUE) })
  mbo1$system.time = mbo1.time
  mbo1$opt.path$env$path$.multifid.lvl = length(e.lvl)
  mbo.res$mbo_expansive = mbo1
  
  # 6. mbo cheapest experiment
  set.seed(e.seed)
  catf("6. mbo cheapest Experiment")
  mbo2.time = system.time( {mbo2 = mbo(fun = getFirst(objfuns), e.par.set, learner = surrogat.model, control = control.common, show.info = TRUE) })
  mbo2$system.time = mbo2.time
  mbo2$opt.path$env$path$.multifid.lvl = 1
  mbo.res$mbo_cheap = mbo2
  
  # 7. multifid
  set.seed(e.seed)
  catf("7. multiFid")
  mbo3.time = system.time({mbo3 = mbo(fun = objfun, par.set = par.set, learner = surrogat.model, control = control.multifid, show.info = TRUE)})
  mbo3$system.time = mbo3.time
  df = as.data.frame(mbo3$opt.path)
  df = df[df$dob > 0,]
  mbo3$perf.steps = table(df$.multifid.lvl, cut(df$dob,3))
  mbo.res$multifid = mbo3
  
  # 8. grid Search
  catf("8. grid Search")
  if (grid.all) {
    grid.lvls = seq_along(e.lvl)
  } else {
    grid.lvls = length(e.lvl)
  }
  grid.res = lapply(grid.lvls, function(lvl) {
    set.seed(e.seed)
    grid.design = generateGridDesign(par.set = e.par.set, resolution = control.common$init.design.points + control.common$iters)
    control.grid = makeMBOControl(iters = 1)
    control.grid$iters = 0
    mbo4.time = system.time({mbo4 = mbo(fun = objfuns[[lvl]], par.set = e.par.set, design = grid.design, learner = surrogat.model, control = control.grid, show.info = TRUE)})
    mbo4$system.time = mbo4.time
    mbo4$opt.path$env$path$.multifid.lvl = lvl
    mbo4
    #mbo.res$grid = mbo4
  })
  names(grid.res) = paste("grid", grid.lvls, sep = ".")
  
  grid.opt.paths = lapply(extractSubList(grid.res, "opt.path", simplify = FALSE), as.data.frame)
  grid.opt.paths.complete = do.call(rbind, grid.opt.paths)
  
  mbo.res = c(mbo.res, grid.res)
  
  
  # 9.0 Calculate thoretical Costs and level count
  for (idx in names(mbo.res)) {
    df = as.data.frame(mbo.res[[idx]]$opt.path)
    mbo.res[[idx]]$theoretical.costs = sum(sapply(df$.multifid.lvl, function(x) control.multifid$multifid.costs(x, tail(e.lvl, 1))))
    mbo.res[[idx]]$level.count = table(factor(df$.multifid.lvl, levels = e.lvl))
  }
  
  # 9. Visualisation
  # preproc grid search
  df.grid = grid.opt.paths.complete
  
  # 9.1 mbo Full + mbo Cheam + grid
  df.grid.1 = rename(df.grid, c("y"="y"))
  op1 = as.data.frame(mbo1$opt.path)
  op2 = as.data.frame(mbo2$opt.path)
  df = rbind(cbind(op1, .multifid.lvl = length(e.lvl)), cbind(op2, .multifid.lvl = 1))
  g = ggplot(df, aes_string(x = getParamIds(e.par.set), y = "y", color = "dob", shape = "as.factor(.multifid.lvl)"))
  g = g + geom_point(size = 4, alpha = 0.6)
  g = g + geom_line(data = df.grid.1, alpha = 0.5, lty = 2, color = "black", mapping = aes(group = .multifid.lvl))
  g
  ggsave(paste0("plots/",e.name,"_mbo1and2.pdf"), width = 8, height = 5)
  
  # 9.2 multiFid + grid
  df.grid.2 = rename(df.grid, c("y"="value"))
  df.grid.2$variable = "response"
  add.g = list(
    geom_line(data = df.grid.2, alpha = 0.5, lty = 2, mapping = aes(group = .multifid.lvl, color = .multifid.lvl)),
    scale_color_gradient2(low = "green", high = "red", mid="blue", midpoint=mean(range(e.lvl)))
  )
  versions = list(
    all = NULL,
    crit = c("response","crit"),
    crit_ei = c("response","crit", "ei", "alpha2"),
    crit_alpha2 = c("response","crit", "alpha2"),
    se_alpha2 = c("se", "alpha2")
  )
  lapply(names(versions), function(v.name) {
    subs = versions[[v.name]]
    p.height = ifelse(is.null(subs), 12, length(subs) * 3)
    pdf(paste0("plots/",e.name, "_multifid_steps_",v.name,".pdf"), width = 10, height = p.height)
    for (i in seq_along(mbo3$plot.data)) {
      plot.data = mbo3$plot.data[[i]]
      alpha1 = collapse(round(unique(plot.data$m.all$value[plot.data$m.all$variable=="alpha1"]), 3), sep = ", ")
      alpha3 = collapse(round(unique(plot.data$m.all$value[plot.data$m.all$variable=="alpha3"]), 3), sep = ", ")
      plot = genGgplot(plot.data, title = sprintf("Step: %i, a1: %s, a3: %s", i, alpha1, alpha3), add.g = add.g, subset.variable = subs)
      print(plot)
    }
    dev.off()
    NULL
  })
  
  # 9.2.5 Multifid Steps As plot (and table)
  df = as.data.frame(mbo.res$multifid$opt.path)
  df = df[df$dob>0,]
  g = ggplot(df, aes(y = .multifid.lvl, x = dob))
  g = g + geom_line() + geom_point(aes(size = y)) 
  ggsave(filename = paste0("plots/", e.name, "_multifid_steps.pdf"), plot = g, width = 7, height = 5)
  
  # 9.3 Compare different methods end result
  df = data.frame(
    method = names(mbo.res),
    x = unlist(extractSubList(mbo.res,"x")),
    y = extractSubList(mbo.res,"y"),
    time = extractSubList(mbo.res,list("system.time",1)),
    theoretical.costs = extractSubList(mbo.res, "theoretical.costs")
  )
  # normalize theoretical costs to speed up from most expenisve method
  df$speedup = df$theoretical.costs / min(df$theoretical.costs)
  g = ggplot()
  g = g + geom_line(data = df.grid.2, alpha = 0.5, mapping = aes_string(x = names(e.par.set$pars), y = "value", group = ".multifid.lvl"))
  g = g + geom_hline(data = df, mapping = aes(yintercept = y, color = method), alpha = 0.5)
  g = g + geom_vline(data = df, mapping = aes(xintercept = x, color = method), alpha = 0.5)
  g1 = g + geom_text(data = df, mapping = aes(x = x, y = y, label = time, color = method, angle = 70, hjust = -0.2))
  ggsave(filename = paste0("plots/", e.name, "_res_compare_system_time.pdf"), plot = g1, width = 10, height = 5)
  g2 = g + geom_text(data = df, mapping = aes(x = x, y = y, label = round(speedup,1), color = method, angle = 70, hjust = -0.2))
  ggsave(filename = paste0("plots/", e.name, "_res_compare_speedup.pdf"), plot = g2, width = 10, height = 5)
  
  # 10 Create a table
  mbo.res$bench.table = data.frame(
    y = extractSubList(mbo.res, element = "y"),
    x = unlist(extractSubList(mbo.res, element = "x")),
    time = extractSubList(mbo.res,list("system.time",1)),
    theoretical.costs = extractSubList(mbo.res, element = "theoretical.costs"),
    lvl.counts = do.call(rbind, extractSubList(mbo.res, element = "level.count", simplify = FALSE))
    )
  
  mbo.res  
}

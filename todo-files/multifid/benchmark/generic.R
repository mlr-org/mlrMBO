library(gridExtra)

generalBenchmark = function(e.name, objfun, e.seed, e.par.set, e.lvl, surrogat.model = NULL, control = NULL, grid.all = TRUE, e.string = NULL, high.res = FALSE, multifid.costs = NULL, gen.plots = TRUE, only.table = FALSE) {
  catf("generalBenchmark for %s", e.name)
  
  if (only.table) gen.plots = FALSE
  # store plots in subdirectory
  if(!is.null(e.string)) {
    dir.create(paste0("../plots/", e.string), showWarnings = TRUE, recursive = TRUE)
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
      show.learner.output = FALSE
    )
    control.common = setMBOControlInfill(
      crit = "ei",
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
    cor.grid.points = 50L,
    costs = multifid.costs,
    force.last.level.evals = 5L,
    generate.plot.data = TRUE
  )
  
  if (is.null(surrogat.model)) {
    surrogat.model = makeLearner("regr.km", predict.type="se", nugget.estim = TRUE, jitter = TRUE)
    #surrogat.model = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)
  }
  # a not so clever surrogat.model to trick randomSearch and gridSearch
  stupid.surrogat.model = giveMeSurrogatLearner("stupid")
  
  #common variables
  nlvls = length(e.lvl)
  
  # generate different objfuns in a list
  objfuns = lapply(seq_along(e.lvl), function(lvl) {
    force(lvl)
    function(x) {
      x$.multifid.lvl = lvl
      objfun(x)
    }
  })
  
  # 4.1 Wrapper + Obj Func
  
  # 5.0 Initiate result list
  mbo.res = list()
  
  # 5. mbo Full experiment
  set.seed(e.seed)
  catf("5. mbo Full Experiment")
  mbo1.time = system.time({
    mbo1 = tryCatch(
      expr = {mbo(fun = getLast(objfuns), e.par.set, learner = surrogat.model, control = control.common, show.info = TRUE) },
      error = function(e) NULL
    )})
  mbo1$system.time = mbo1.time
  mbo1$opt.path$env$path$.multifid.lvl = nlvls
  mbo.res$mbo_expensive = mbo1
  
  # 6. mbo cheapest experiment
  set.seed(e.seed)
  catf("6. mbo cheapest Experiment")
  mbo2.time = system.time({
    mbo2 = tryCatch(
      expr = {mbo(fun = getFirst(objfuns), e.par.set, learner = surrogat.model, control = control.common, show.info = TRUE)},
      error = function(e) NULL
    )})
  mbo2$system.time = mbo2.time
  mbo2$opt.path$env$path$.multifid.lvl = 1
  mbo2$y = getLast(objfuns)(c(trafoValue(e.par.set, mbo2$x), list(.multifid.lvl = nlvls)))
  mbo.res$mbo_cheap = mbo2
  
  # 7. multifid
  set.seed(e.seed)
  catf("7. multiFid")
  mbo3.time = system.time({
    mbo3 = tryCatch(
      expr = {mbo(fun = objfun, par.set = e.par.set, learner = surrogat.model, control = control.multifid, show.info = TRUE)},
      error = function(e) NULL
    )})
  mbo3$system.time = mbo3.time
  if (!is.null(mbo3)) {
    df = as.data.frame(mbo3$opt.path)
    df = df[df$dob > 0,]
    mbo3$perf.steps = table(df$.multifid.lvl, cut(df$dob,3))  
  }
  mbo.res$multifid = mbo3
  
  # 8a Random Search
  catf("8a. random Search")
  random.design = generateRandomDesign(par.set = e.par.set, n = control.common$init.design.points + control.common$iters - 1) #1 mbo eval
  random.control = control.common
  random.control$iters = 0L
  mbo5.time = system.time({
    mbo5 = tryCatch(
      expr = {mbo(fun = getLast(objfuns), par.set = e.par.set, design = random.design, learner = stupid.surrogat.model, control = random.control, show.info = TRUE)},
      error = function(e) NULL
    )})
  mbo5$system.time = mbo5.time
  mbo5$opt.path$env$path$.multifid.lvl = nlvls
  mbo.res$random = mbo5
  
  # 8b grid Search
  catf("8b. grid Search")
  if (grid.all) {
    grid.lvls = seq_along(e.lvl)
  } else {
    grid.lvls = nlvls
  }
  grid.res = lapply(grid.lvls, function(lvl) {
    set.seed(e.seed)
    resolution = c(control.common$init.design.points, control.common$iters)
    if (!high.res || sum(getParamLengths(e.par.set)) == 1) 
      resolution = ceiling(sum(resolution)^(1/sum(getParamLengths(e.par.set))))
    else
      resolution = resolution[1]
    grid.design = generateGridDesign(par.set = e.par.set, resolution = resolution)
    grid.control = control.common
    grid.control$iters = 0
    mbo4.time = system.time({
      mbo4 = tryCatch(
        expr = {mbo(fun = objfuns[[lvl]], par.set = e.par.set, design = grid.design, learner = stupid.surrogat.model, control = grid.control, show.info = TRUE)},
        error = function(e) NULL
      )})
    mbo4$system.time = mbo4.time
    mbo4$opt.path$env$path$.multifid.lvl = lvl
    if(lvl < nlvls) {
      mbo4$y = getLast(objfuns)(c(trafoValue(e.par.set, mbo4$x), list(.multifid.lvl = nlvls)))
    }
    mbo4
    #mbo.res$grid = mbo4
  })
  names(grid.res) = paste("grid", grid.lvls, sep = ".")
  
  grid.opt.path.dfs = lapply(extractSubList(grid.res, "opt.path", simplify = FALSE), as.data.frame)
  grid.opt.path.df.complete = do.call(rbind, grid.opt.path.dfs)
  
  mbo.res = c(mbo.res, grid.res)
  
  
  # 9.0 Calculate thoretical Costs and level count
  for (idx in names(mbo.res)) {
    df = as.data.frame(mbo.res[[idx]]$opt.path)
    if(is.null(control.multifid$multifid.costs))
      cost.vector = (e.lvl)^2
    else
      cost.vector = control.multifid$multifid.costs
    mbo.res[[idx]]$theoretical.costs = sum(cost.vector[df$.multifid.lvl])
    mbo.res[[idx]]$level.count = table(factor(df$.multifid.lvl, levels = seq_along(e.lvl)))
  }
  
  # 9. Visualisation
  
  if(gen.plots && sum(getParamLengths(e.par.set)) == 1) {
    
    #### 1d Visualisierung ####
    
    # preproc grid search
    
    # 9.1 mbo Full + mbo Cheap + grid
    g = genPlotCompareMbos(
      opt.path.grids = extractSubList(grid.res, "opt.path", simplify = FALSE),
      opt.path.cheap = mbo.res$mbo_cheap$opt.path,
      opt.path.expensive = mbo.res$mbo_expensive$opt.path
      )
    ggsave(plot = g, filename = paste0("../plots/",e.name,"_mbo1and2.pdf"), width = 8, height = 5)
    
    # 9.2 multiFid + grid
    df.grid.2 = rename(grid.opt.path.df.complete, c("y"="value"))
    df.grid.2$variable = "y"
    add.g = list(
      geom_line(data = df.grid.2, alpha = 0.5, lty = 2, mapping = aes(group = .multifid.lvl, color = as.factor(.multifid.lvl)))
    )
    versions = list(
      all = c("y", "crit", "ei", "alpha1", "alpha2", "alpha3", "se"),
      crit = c("y","crit"),
      crit_ei = c("y","crit", "ei", "alpha2"),
      crit_alpha2 = c("y","crit", "alpha2"),
      se_alpha2_3 = c("y", "se", "alpha2", "alpha3")
    )
    lapply(names(versions), function(v.name) {
      subs = versions[[v.name]]
      p.height = ifelse(is.null(subs), 12, length(subs) * 2 + 2)
      pdf(paste0("../plots/",e.name, "_multifid_steps_",v.name,".pdf"), width = 10, height = p.height)
      for (i in seq_along(mbo.res$multifid$plot.data)) {
        plot.data = mbo.res$multifid$plot.data[[i]]
        alpha1 = collapse(round(daply(plot.data$all, ".multifid.lvl", function(x) getFirst(x$alpha1)), 3), sep = ", ")
        sd = collapse(round(daply(plot.data$all, ".multifid.lvl", function(x) getFirst(x$sd)), 3), sep = ", ")
        plot = mlrMBO:::plotMultiFidStep(plot.data, title = sprintf("Step: %i, a1: %s, sd: %s", i, alpha1, sd), add.g = add.g, subset.variable = subs)
        print(plot)
      }
      dev.off()
    })
    
    # 9.2.5 Multifid Steps As plot (and table)
    g = genPlotSteps(mbo.res$multifid$opt.path)
    ggsave(plot = g, filename = paste0("../plots/", e.name, "_multifid_steps.pdf"), width = 7, height = 5)
    
    # 9.3 Compare different methods end result
    final.points = data.frame(
      method = names(mbo.res),
      x = unlist(extractSubList(mbo.res,"x")),
      y = extractSubList(mbo.res,"y")
    )
    # normalize theoretical costs to speed up from most expenisve method
    g = genPlotOptPoints(extractSubList(grid.res, "opt.path", simplify = FALSE), 
                         opt.paths = extractSubList(xx.mbo.res[- grep(pattern = "grid", x = names(xx.mbo.res))], "opt.path", simplify = FALSE),
                         final.points = final.points)
    ggsave(plot = g, filename = paste0("../plots/", e.name, "_res_compare.pdf"), width = 10, height = 5)
    
  } else if (gen.plots && sum(getParamLengths(e.par.set)) == 2) {
    
    #### 2d plots ####
    
    versions = list(
      basic = c("y", "crit"), 
      alphas = c("y", "crit", "alpha2", "alpha3"),
      extended = c("y", "ei", "se", "alpha2", "alpha3"))
    lapply(names(versions), function(v.name) {
      subs = versions[[v.name]]
      p.width = length(subs) * 4
      pdf(paste0("../plots/",e.name, "_multifid_steps_",v.name,".pdf"), width = p.width, height = 10)
      for (i in seq_along(mbo.res$multifid$plot.data)) {
        plot.data = mbo.res$multifid$plot.data[[i]]
        alpha1 = collapse(round(unique(plot.data$all$alpha1), 3), sep = ", ")
        sd = collapse(round(unique(plot.data$all$sd), 3), sep = ", ")
        plot = mlrMBO:::plotMultiFidStep(plot.data, title = sprintf("Step: %i, a1: %s, sd: %s", i, alpha1, sd), subset.variable = subs)
        print(plot)
      }
      dev.off()
    })
    
    ## Final state
    plot.data = getLast(mbo.res$multifid$plot.data) #last step
    plots = mlrMBO:::plotMultiFidStep2dRaw(plot.data, subset.variable = c("y", "crit"))
    m.spec = rename(grid.opt.path.df.complete, c("y" = "value"))
    m.spec = m.spec[m.spec$dob == 0, ] #remove proposed values for correct geom_grid()
    m.spec$variable = "real"
    xname = plot.data$xname
    g.real = mlrMBO:::plotMultiFidStep2dRawEach(m.spec, xname, plot.data$old.points[,c(xname, ".multifid.lvl")], plot.data$best.points[,c(xname, ".multifid.lvl")] )
    pdf(paste0("../plots/",e.name, "_multifid_final.pdf"), width = 8, height = 10)
      gs = do.call(grid.arrange, c(c(plots, list(g.real)), list(nrow = 1, main = "Final Stage")))
      print(gs)
    dev.off()
  }
  
  # 10 Create a table
  mbo.res$bench.table = cbind.data.frame(
    y = extractSubList(mbo.res, element = "y"),
    x = convertListOfRowsToDataFrame(extractSubList(mbo.res, element = "x", simplify = FALSE)),
    system.time = extractSubList(mbo.res, list("system.time",1), simplify = TRUE),
    eval.time = sapply(extractSubList(mbo.res, list("opt.path", "env", "exec.time")), sum),
    theoretical.costs = extractSubList(mbo.res, element = "theoretical.costs"),
    lvl.counts = do.call(rbind, extractSubList(mbo.res, element = "level.count", simplify = FALSE))
    )
  df = mbo.res$bench.table
  num.col = sapply(df, is.numeric)
  df[,num.col] = sapply(df[,num.col], function(x) sprintf("%g", x))
  pdf(paste0("../plots/",e.name, "_compare_table.pdf"), width = 14, height = 10)
    grid.table(df)
  dev.off()
  
  if (only.table)
    mbo.res["bench.table"]
  else
    mbo.res  
}

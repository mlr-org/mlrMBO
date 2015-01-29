generalBenchmark = function(e.name, objfun, e.seed, e.par.set, e.lvl, surrogat.model = NULL, control = NULL, grid.all = TRUE, e.string = NULL, high.res = FALSE) {
  # store plots in subdirectory
  if(!is.null(e.string)) {
    dir.create(paste0("plots/", e.string), showWarnings = FALSE)
    e.name = paste0(e.string, "/", e.name)
  }
  # initialize control if not sets
  if (is.null(control)) {
    # 4. common parameters
    control.common = makeMBOControl(
      init.design.points = 20L, #distributed over the different levels, seems not to work for <5 each
      init.design.fun = maximinLHS,
      iters = 5L,
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
    cor.grid.points = 40L
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
  
  # 5.0 Initiate result list
  mbo.res = list()
  
  # 5. mbo Full experiment
  set.seed(e.seed)
  catf("5. mbo Full Experiment")
  mbo1.time = system.time( {mbo1 = mbo(fun = getLast(objfuns), e.par.set, learner = surrogat.model, control = control.common, show.info = TRUE) })
  mbo1$system.time = mbo1.time
  mbo1$opt.path$env$path$.multifid.lvl = length(e.lvl)
  mbo.res$mbo_expensive = mbo1
  
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
  mbo3.time = system.time({mbo3 = mbo(fun = objfun, par.set = e.par.set, learner = surrogat.model, control = control.multifid, show.info = TRUE)})
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
    resolution = control.common$init.design.points + control.common$iters
    if (!high.res) 
      resolution = ceiling((resolution)^(1/sum(getParamLengths(e.par.set))))
    grid.design = generateGridDesign(par.set = e.par.set, resolution = resolution)
    control.grid = makeMBOControl(iters = 1)
    control.grid$iters = 0
    mbo4.time = system.time({mbo4 = mbo(fun = objfuns[[lvl]], par.set = e.par.set, design = grid.design, learner = surrogat.model, control = control.grid, show.info = TRUE)})
    mbo4$system.time = mbo4.time
    mbo4$opt.path$env$path$.multifid.lvl = lvl
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
      cost.vector = length(e.lvl)/seq_along(e.lvl)
    else
      cost.vector = control.multifid$multifid.costs
    mbo.res[[idx]]$theoretical.costs = sum(cost.vector[df$.multifid.lvl])
    mbo.res[[idx]]$level.count = table(factor(df$.multifid.lvl, levels = e.lvl))
  }
  
  # 9. Visualisation
  
  if(sum(getParamLengths(e.par.set)) == 1) {
    
    #### 1d Visualisierung ####
    
    # preproc grid search
    
    # 9.1 mbo Full + mbo Cheap + grid
    g = genPlotCompareMbos(
      opt.path.grids = extractSubList(grid.res, "opt.path", simplify = FALSE),
      opt.path.cheap = extractSubList(mbo.res$mbo_cheap, "opt.path", simplify = FALSE),
      opt.path.expensive = extractSubList(mbo.res$mbo_expensive, "opt.path", simplify = FALSE)
      )
    ggsave(g, paste0("plots/",e.name,"_mbo1and2.pdf"), width = 8, height = 5)
    
    # 9.2 multiFid + grid
    df.grid.2 = rename(grid.opt.path.df.complete, c("y"="value"))
    df.grid.2$variable = "y"
    add.g = list(
      geom_line(data = df.grid.2, alpha = 0.5, lty = 2, mapping = aes(group = .multifid.lvl, color = as.factor(.multifid.lvl))),
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
      for (i in seq_along(mbo.res$multifid$plot.data)) {
        plot.data = mbo.res$multifid$plot.data[[i]]
        alpha1 = collapse(round(unique(plot.data$m.all$value[plot.data$m.all$variable=="alpha1"]), 3), sep = ", ")
        alpha3 = collapse(round(unique(plot.data$m.all$value[plot.data$m.all$variable=="alpha3"]), 3), sep = ", ")
        plot = genGgplot(plot.data, title = sprintf("Step: %i, a1: %s, a3: %s", i, alpha1, alpha3), add.g = add.g, subset.variable = subs)
        print(plot)
      }
      dev.off()
    })
    
    # 9.2.5 Multifid Steps As plot (and table)
    g = genPlotSteps(mbo.res$multifid$opt.path)
    ggsave(g, filename = paste0("plots/", e.name, "_multifid_steps.pdf"), plot = g, width = 7, height = 5)
    
    # 9.3 Compare different methods end result
    final.points = data.frame(
      method = names(mbo.res),
      x = unlist(extractSubList(mbo.res,"x")),
      y = extractSubList(mbo.res,"y")
    )
    # normalize theoretical costs to speed up from most expenisve method
    g = genPlotOptPoints(extractSubList(grid.res, "opt.path", simplify = FALSE), dropNamed())
    ggsave(g, filename = paste0("plots/", e.name, "_res_compare_system_time.pdf"), plot = g1, width = 10, height = 5)
    
  } else if (sum(getParamLengths(e.par.set)) == 2) {
    
    #### 2d plots ####
    
    versions = list(basic = c("y", "crit"), extended = c("y", "ei", "alpha2"))
    lapply(names(versions), function(v.name) {
      subs = versions[[v.name]]
      p.width = length(subs) * 4
      pdf(paste0("plots/",e.name, "_multifid_steps_",v.name,".pdf"), width = p.width, height = 10)
      for (i in seq_along(mbo.res$multifid$plot.data)) {
        plot.data = mbo.res$multifid$plot.data[[i]]
        alpha1 = collapse(round(unique(plot.data$all$alpha1), 3), sep = ", ")
        alpha3 = collapse(round(unique(plot.data$all$alpha3), 3), sep = ", ")
        plot = genGgplot(plot.data, title = sprintf("Step: %i, a1: %s, a3: %s", i, alpha1, alpha3), subset.variable = subs)
        print(plot)
      }
      dev.off()
    })
    
    ## Final state
    plot.data = getLast(mbo.res$multifid$plot.data) #last step
    plots = genGgplot2dRaw(plot.data, subset.variable = c("y", "crit"))
    m.spec = rename(grid.opt.path.df.complete, c("y" = "value"))
    m.spec = m.spec[m.spec$dob == 0, ] #remove proposed values for correct geom_grid()
    m.spec$variable = "real"
    xname = plot.data$xname
    g.real = genGgplot2dRawEach(m.spec, xname, plot.data$old.points[,c(xname, ".multifid.lvl")], plot.data$best.points[,c(xname, ".multifid.lvl")] )
    pdf(paste0("plots/",e.name, "_multifid_final.pdf"), width = 8, height = 10)
      gs = do.call(grid.arrange, c(c(plots, list(g.real)), list(nrow = 1, main = "Final Stage")))
      print(gs)
    dev.off()
  }
  # 10 Create a table
  mbo.res$bench.table = data.frame(
    y = extractSubList(mbo.res, element = "y"),
    x = convertListOfRowsToDataFrame(extractSubList(mbo.res, element = "x", simplify = FALSE)),
    time = extractSubList(mbo.res,list("system.time",1)),
    theoretical.costs = extractSubList(mbo.res, element = "theoretical.costs"),
    lvl.counts = do.call(rbind, extractSubList(mbo.res, element = "level.count", simplify = FALSE))
    )
  
  mbo.res  
}

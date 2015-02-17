### Define Learners
giveMeTasks = function(x = NULL) {
  #Covertype http://openml.org/d/150
  #multiclass numeric and 01 factors
  #581012 inst & 54 features
  covtype.f = function() {makeClassifTask(id = "covtype", data = read.arff("../data/covtype-normalized.arff"), target = "class")}
  
  #Covertype http://openml.org/d/180
  #multiclass numeric and 01 factors
  #110393 inst & 54 features
  covertype.f = function() {makeClassifTask(id = "covertype", data = read.arff("../data/dataset_184_covertype.arff"), target = "class")} 
  
  #BNG CMC http://openml.org/d/255
  #multiclass numeric and factors
  #55296 inst & 10 features
  #not a very good svm data set
  bng_cmc.f = function() {
    data = read.arff("../data/BNG_cmc.arff")
    colnames(data) = make.names(colnames(data))
    makeClassifTask(id = "bng_cmc", data = data, target = "Contraceptive_method_used") 
  }
  
  #meta steam intervals OpenML
  #multiclass numeric
  meta_stream_intervals.f = function() {
    data = read.arff("../data/meta_stream_intervals.arff")
    colnames(data) = make.names(colnames(data))
    makeClassifTask(id = "meta_stream_intervals", data = data, target = "class")
  } 
  
  task.list = list(
    sonar = function() {sonar.task},
    iris = function() {iris.task},
    
    #http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary.html
    #binary
    w7a = function() {makeClassifTask(id = "w7a", data = libsvm.read("../data/w7a"), target = "Y")},
    w8a = function() {makeClassifTask(id = "w8a", data = libsvm.read("../data/w8a"), target = "Y")},
    covtype.binary = function() {makeClassifTask(id = "covtype.binary", data = libsvm.read("../data/covtype.libsvm.binary"), target = "Y")},
    
    covtype = covtype.f,
    covtype.dummy = function() {createDummyFeatures(covtype.f(), method = "reference")},
    covertype = covertype.f,
    covertype.dummy = function() {createDummyFeatures(covertype.f(), method = "reference")},
    bng_cmc = bng_cmc.f(),
    bng_cmc.dummy = function() {createDummyFeatures(bng_cmc.f(), method = "reference")},
    meta_stream_intervals = meta_stream_intervals.f
    # clickprediction = function()
  )
  if (is.null(x))
    x = names(task.list)
  assertSubset(x, names(task.list))
  lapply(x, function(name) do.call(task.list[[name]], list()))
}

giveMeLearners = function(x = NULL) {
  lrns = list(
    LiblineaRMultiClass = makeLearner("classif.LiblineaRMultiClass"),
    LiblineaRBinary = makeLearner("classif.LiblineaRBinary", type = 1),
    svm = makeLearner("classif.svm")
  )
  if (is.null(x))
    x = names(task.list)
  lrns[x]
}

giveMeParamSets = function(lrns) {
  # lrns.ids = sapply(strsplit(extractSubList(lrns, "id"), "\\."), getLast)
  lrns.ids = extractSubList(lrns, "id")
  LiblineaR = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("epsilon", lower = -20, upper = 2, trafo = function(x) 2^x))
  svm = makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("gamma", lower = -10, upper = 6, trafo = function(x) 2^x),
    makeNumericParam("tolerance", lower = -10, upper = -1, trafo = function(x) log(1+2^x) )
  )
  par.set.list = list(
    classif.LiblineaRMultiClass = LiblineaR,
    classif.LiblineaRBinary = LiblineaR,
    classif.svm = svm,
    regr.svm = list(svm, makeParamSet(
      makeNumericParam("epsilon", lower = -10, upper = 6, trafo = function(x) 2^x)))
  )
  par.set.list[lrns.ids]
}

giveMeMBOControl = function(e.lvls = giveMeLvl("std"), budget = 50L) {
  init.design.points = length(e.lvls) * 4
  ctrl = makeMBOControl(
    init.design.points = init.design.points, 
    init.design.fun = maximinLHS,
    iters = max(1L, budget - init.design.points),
    on.learner.error = "stop",
    show.learner.output = FALSE
  )
  ctrl = setMBOControlInfill(
    control = ctrl, 
    opt = "focussearch", 
    opt.restarts = 1L, 
    opt.focussearch.maxit = 1L, 
    opt.focussearch.points = 100L,
    filter.proposed.points = TRUE,
    filter.proposed.points.tol = 0.0005
  )
  ctrl
}

giveMeMBOMultiFidControl = function(ctrl = NULL, e.lvls = giveMeLvl("std"), costs = NULL, ...) {
  if (is.null(ctrl))
      ctrl = giveMeMBOControl(e.lvls = e.lvls, ...)
  ctrl = setMBOControlMultiFid(
    control = ctrl, 
    param = "dw.perc", 
    lvls = e.lvls,
    cor.grid.points = 40L,
    costs = NULL
  )
  ctrl$infill.crit = "multiFid"
  ctrl
}

giveMeLvl = function(x = 1) {
  lvl.list = list(
    std = c(0.1, 0.2, 0.5, 1),
    big.data = c(0.025, 0.05, 0.1, 0.2, 1),
    simple = c(0.5, 1)
    )
  lvl.list[[x]]
}

giveMeSurrogatLearner = function(x = 1) {
  sur.list = list(
    surrogat.learner = makeLearner("regr.km", nugget.estim = TRUE, jitter = TRUE)
    )
  sur.list[[x]]
}

giveMeTuneControls = function (budget, surrogat.learner = giveMeSurrogatLearner()) {
  tune.controls = list(
    mlr.multiFid.control = mlr:::makeTuneControlMBO(
      mbo.control = giveMeMBOMultiFidControl(budget = budget), 
      learner = surrogat.learner),
    mlr.multiFid.fixedCosts.control = mlr:::makeTuneControlMBO(
      mbo.control = giveMeMBOMultiFidControl(
        e.lvls = giveMeLvl("std"), 
        costs = giveMeLvl("std")^2,
        budget = budget), 
      learner = surrogat.learner),
    mlr.multiFid.bigData.control = mlr:::makeTuneControlMBO(
      mbo.control = giveMeMBOMultiFidControl(
        e.lvls = giveMeLvl("big.data"),
        budget = budget), 
      learner = surrogat.learner),
    mlr.mlrMBO.control = mlr:::makeTuneControlMBO(
      mbo.control = giveMeMBOControl(budget = budget), 
      learner = surrogat.learner)
  )
  tune.controls = c(tune.controls, list(
    mlr.tuneRandom.control = makeTuneControlRandom(maxit = giveMeResolution(tune.controls$mlr.multiFid.control)))
  )
  tune.controls
}

giveMeTunedLearners2 = function (learners, tune.controls, rsi, measures = mmce, par.sets = NULL, show.info = TRUE, dw.perc = 1) {
  if (is.null(par.sets))
    par.sets = giveMeParamSets(learners)
  assertList(par.sets, len = length(learners))
  assertSubset(names(par.sets), extractSubList(learners, "id"))
  res = lapply(learners, function(lrn) {
    lapply(names(tune.controls), function(tune.ctrl.name) {
      lrn.id = lrn$id
      lrn = makeDownsampleWrapper(learner = lrn, dw.perc = dw.perc, dw.stratify = TRUE)
      lrn = makeTuneWrapper(learner = lrn, resampling = rsi, measures = measures, par.set = par.sets[[lrn.id]], control = tune.controls[[tune.ctrl.name]], show.info = show.info)
      lrn$id = paste0(lrn.id, ".", tune.ctrl.name)
      return(lrn)
    })
  })
  unlist(res, recursive = FALSE)
}

giveMeTunedLearners = function(learners, tune.controls, rsi, measures = mmce, par.sets = NULL, show.info = TRUE) {
  high = giveMeTunedLearners2(learners, tune.controls, rsi, measures = measures, par.sets = par.sets, show.info = show.info)
  is.mf = unlist(extractSubList(tune.controls, c("mbo.control","multifid"))) #gets rid of NULLs
  is.mf = names(is.mf[is.mf]) #only take true
  is.mf = names(tune.controls) %in% is.mf
  tune.controls = tune.controls[!is.mf] #subset to not multifid tune objects
  names(tune.controls) = paste0(names(tune.controls),".lowFidelity")
  low = giveMeTunedLearners2(learners, tune.controls, rsi, measures = measures, par.sets = par.sets, show.info = show.info, dw.perc = getFirst(giveMeLvl()))
  c(high, low)
}

giveMeResampleDesc = function(x = 1, ...) {
  rdesc.list = list(
    subsample = makeResampleDesc("Subsample", iters = 2, ...),
    cv = makeResampleDesc("CV", iters = 10, ...),
    holdout = makeResampleDesc("Holdout", ...),
    inner = makeResampleDesc("Holdout"),
    outer = makeResampleDesc("Subsample", iters = 2)
  )
  rdesc.list[[x]]
}

giveMeResolution = function(mlr.ctrl) {
  mlr.ctrl$mbo.control$init.design.points + mlr.ctrl$mbo.control$iters
}

giveMePlot = function(tune.rres2, init.design.points = NULL, cut.dob = -1L, ...) {
  all.df = melt(extractSubList(tune.rres2, "ops.df", simplify = FALSE), measure.vars = c("mmce.test.mean.best", "mmce.test.mean.best.index", "exec.time.cum"))
  all.df = rename(all.df, c("L1" = "learner.id"))
  all.df = all.df[all.df$dob >= cut.dob, ]
  g = ggplot(data = all.df, aes(y = value, x = dob, color = learner.id))
  g = g + geom_line(size = 1, alpha = 0.1, mapping = aes(group = paste0(iter,learner.id)))
  g = g + facet_wrap(~variable, scales = "free")
  if (!is.null(init.design.points))
    g = g + geom_vline(alpha = 0.5, xintercept = init.design.points)
  g = g + stat_summary(fun.y=mean, geom="line", size = 1.5, alpha = 0.8, mapping = aes(group = learner.id))
  g = giveMeGgExtras(g = g, ...)
  return(g)
}

giveMePareto = function(tune.rres2, ...) {
  exec.times = melt(extractSubList(tune.rres2, "exec.times", simplify = FALSE), value.name = "exec.times")
  exec.times = rename(exec.times, c("L1" = "learner.id"))
  all.df = cbind.data.frame(
    exec.times,
    rbind.fill(extractSubList(tune.rres2, "measures.test", simplify = FALSE))
  )
  exec.times.mean = melt(extractSubList(tune.rres2, "exec.times.mean", simplify = FALSE), value.name = "exec.times")
  all.df2 = cbind.data.frame(
    exec.times.mean,
    convertListOfRowsToDataFrame(extractSubList(tune.rres2, "aggr", simplify = FALSE))
  )
  all.df2 = rename(all.df2, c("L1" = "learner.id", "mmce.test.mean" = "mmce"))
  g = ggplot(data = all.df, aes(x = mmce, y = exec.times, color = learner.id, group = learner.id))
  g = g + geom_point(alpha = 0.2, size = 3)
  g = g + geom_point(data = all.df2, size = 5, alpha = 0.9)
  g = giveMeGgExtras(g = g, ...)
  return(g)
}

giveMeVisuals = function (all.res, e.string, init.design.points = NULL) {
  task.ids = extractSubList(all.res, c("task.id"))
  learner.ids = extractSubList(all.res, c("opt.result", "learner", "next.learner", "id"))
  combs = unique(data.frame(task.ids, learner.ids))
  
  visualize = function(task.id, learner.id) {
    res.by.task.learner = all.res[task.ids == task.id & learner.ids == learner.id]
    res.by.resampling = split(res.by.task.learner, extractSubList(res.by.task.learner, "learner.id"))
    this.e.string = paste0(e.string,"/", task.id, "_", learner.id)
    
    tune.rres2 = lapply(res.by.resampling, function(rres) {
      res = list()
      ops = extractSubList(rres, c("opt.result", "opt.path"), simplify = FALSE)
      res$exec.times = vnapply(ops, function(op) sum(getOptPathExecTimes(op)))
      res$exec.times.mean = c(exec.times.mean = mean(res$exec.times))
      res$exec.times.sd = c(exec.times.sd = sd(res$exec.times))
      res$measures.test = convertListOfRowsToDataFrame(extractSubList(rres, "performance", simplify = FALSE))
      res$aggr = vnapply(res$measures.test, mean)
      names(res$aggr) = paste0(names(res$aggr),".test.mean")
      res$measures.test.sd = vnapply(res$measures.test, sd)
      names(res$measures.test.sd) = paste0(names(res$aggr),".test.sd")
      res$op.dfs = OpsAddByIter(ops, best.col = "mmce.test.mean")
      res$best.reached.at = vnapply(res$op.dfs, function(df) getLast(df$mmce.test.mean.best.index))
      res$best.reached.at.mean = mean(res$best.reached.at)
      res$ops.df = do.call(rbind, res$op.dfs)
      return(res)
    })
    
    #shorten names of learners
    names(tune.rres2) = substr(names(tune.rres2), start = nchar(learner.id)+2, stop = 99)
    common.title = paste(task.id, learner.id)
    
    pdf(paste0("../plots/",this.e.string,"_CV_compare_table.pdf"), width = 20, height = 5)
    giveMeGridTable(giveMeResultTable(tune.rres2, pretty = TRUE), title = common.title)
    dev.off()
    
    g = giveMePlot(tune.rres2, init.design.points = init.design.points, title = common.title)
    ggsave(plot = g, filename = paste0("../plots/",this.e.string,"_CV_compare_increase.pdf"), width = 14, height= 6 )
    
    g = giveMePlot(tune.rres2, init.design.points = init.design.points, cut.dob = 10L, title = common.title)
    ggsave(plot = g, filename = paste0("../plots/",this.e.string,"_CV_compare_increase_shortened.pdf"), width = 14, height = 6)
    
    g = giveMePareto(tune.rres2, title = paste(task.id, learner.id))
    ggsave(plot = g, filename = paste0("../plots/",this.e.string,"_CV_compare_pareto.pdf"), width = 10, height = 6)
    
    invisible(NULL)
  } 
  apply(combs, 1, function(x) visualize(x["task.ids"], x["learner.ids"]))
}


giveMeGridTable = function (d, title = "", footnote = "") {
  table = tableGrob(d)
  grid.newpage()
  h = grobHeight(table)
  w = grobWidth(table)
  title = textGrob(title, y=unit(0.5,"npc") + 0.5*h, 
                    vjust=0, gp=gpar(fontsize=20))
  footnote = textGrob(footnote, 
                       x=unit(0.5,"npc") - 0.5*w,
                       y=unit(0.5,"npc") - 0.5*h, 
                       vjust=1, hjust=0,gp=gpar( fontface="italic"))
  gt = gTree(children=gList(table, title, footnote))
  return(grid.draw(gt))
}

giveMeResultTable = function(tune.rres2, pretty = TRUE) {
  #result df preparation
  res.df = cbind.data.frame(
    convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("aggr"), simplify = FALSE)),
    convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("measures.test.sd"), simplify = FALSE)),
    convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("exec.times.mean"), simplify = FALSE)),
    convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("exec.times.sd"), simplify = FALSE)),
    convertListOfRowsToDataFrame(extractSubList(tune.rres2, c("best.reached.at.mean"), simplify = FALSE), col.names = "best.reached.at.mean")
  )
  res.df
  if (pretty) {
    num.col = sapply(res.df, is.numeric)
    res.df[,num.col] = sapply(res.df[,num.col], function(x) sprintf("%.4g", x))
  }
  return(res.df)
}

giveMeGgExtras = function(g, title = character(0), ...) {
  add.g = list(...)
  for (i in seq_along(add.g)) {
    g = g + add.g[[i]]
  }
  #g = g + scale_colour_brewer(palette="Set1")
  g = g + scale_color_manual(values = c("#7F0000", "#FF3A39", "#1BE800", "#246616", "#A2E58C", "#0056CF", "#4C94FF"))
  g = g + theme_bw() + ggtitle(label = title)
  return(g)
}
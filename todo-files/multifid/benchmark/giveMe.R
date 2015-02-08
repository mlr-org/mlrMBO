### Define Learners
giveMeTasks = function(x = NULL) {
  covtype.f = function() {makeClassifTask(id = "covtype", data = read.arff("../data/covtype-normalized.arff"), target = "class")}
  task.list = list(
    sonar = function() {sonar.task},
    w7a = function() {makeClassifTask(id = "w7a", data = libsvm.read("../data/w7a"), target = "Y")},
    w8a = function() {makeClassifTask(id = "w8a", data = libsvm.read("../data/w8a"), target = "Y")},
    covtype = covtype.f,
    iris = function() {iris.task},
    covtype.dummy = function() {covtype.dummy = createDummyFeatures(covtype.f())},
    covtype.binary = function() {makeClassifTask(id = "covtype.binary", data = libsvm.read("../data/covtype.libsvm.binary"), target = "Y")}
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
  par.set.list = list(
    classif.LiblineaRMultiClass = LiblineaR,
    classif.LiblineaRBinary = LiblineaR,
    classif.svm = makeParamSet(
      makeNumericParam("cost", lower = -15, upper = 10, trafo = function(x) 2^x),
      makeNumericParam("nu", lower = -10, upper = 10))
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

giveMeTunedLearners = function (learners, tune.controls, rsi, measures = mmce, par.sets = NULL, show.info = TRUE) {
  if (is.null(par.sets))
    par.sets = giveMeParamSets(learners)
  assertList(par.sets, len = length(learners))
  assertSubset(names(par.sets), extractSubList(learners, "id"))
  res = lapply(learners, function(lrn) {
    lapply(names(tune.controls), function(tune.ctrl.name) {
      lrn.id = lrn$id
      lrn = makeDownsampleWrapper(learner = lrn, dw.perc = 1, dw.stratify = TRUE)
      lrn = makeTuneWrapper(learner = lrn, resampling = rsi, measures = measures, par.set = par.sets[[lrn.id]], control = tune.controls[[tune.ctrl.name]], show.info = show.info)
      lrn$id = paste0(lrn.id, ".", tune.ctrl.name)
      return(lrn)
    })
  })
  unlist(res, recursive = FALSE)
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

giveMePlot = function(all.df) {
  g = ggplot(data = all.df, aes(y = value, x = dob, color = learner.id))
  g = g + geom_line(siez = 1, alpha = 0.5, mapping = aes(group = paste0(iter,learner.id)))
  g = g + facet_wrap(~variable, scales = "free")
  g = g + stat_summary(fun.y=mean, geom="line", size = 2, alpha = 0.9, mapping = aes(group = learner.id))
  g = g + theme_bw()
  return(g)
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
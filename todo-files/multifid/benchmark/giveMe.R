### Define Learners
giveMeTasks = function(x = NULL) {
  covtype.f = function() {makeClassifTask(id = "covtype", data = read.arff("../data/covtype-normalized.arff"), target = "class")}
  task.list = list(
    sonar = function() {sonar.task},
    w7a = function() {makeClassifTask(id = "w7a", data = libsvm.read("../data/w7a"), target = "Y")},
    w8a = function() {makeClassifTask(id = "w8a", data = libsvm.read("../data/w8a"), target = "Y")},
    covtype = covtype.f,
    iris = function() {iris.task},
    covtype.dummy = function() {covtype.dummy = createDummyFeatures(covtype.f())}
  )
  if (is.null(x))
    x = names(task.list)
  assertSubset(x, names(task.list))
  lapply(x, function(name) do.call(task.list[[name]], list()))
}

giveMeLearners = function(x = NULL) {
  lrns = list(
    LiblineaRMultiClass = makeLearner("classif.LiblineaRMultiClass"),
    liblineaRBinary = makeLearner("classif.LiblineaRBinary", type = 1),
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
      makeNumericParam("C", lower = -5, upper = 2, trafo = function(x) 2^x),
      makeNumericParam("sigma", lower = -3, upper = 3, trafo = function(x) 2^x))
  )
  par.set.list[lrns.ids]
}

giveMeMBOControl = function(e.lvls = giveMeLvl("std"), budget = 50L) {
  init.design.points = length(e.lvls) * 4
  ctrl = makeMBOControl(
    init.design.points = init.design.points, 
    init.design.fun = maximinLHS,
    iters = budget - init.design.points,
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

giveMeMBOMultiFidControl = function(ctrl = NULL, e.lvls = giveMeLvl("std"), costs = NULL) {
  if (is.null(ctrl))
      ctrl = giveMeMBOControl(e.lvls = e.lvls)
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
  if (is.null(par.set))
    par.sets = giveMeParamSets(learners)
  assertList(par.sets, len = length(learners))
  assertSubset(names(par.sets), extractSubList(learers, "id"))
  lapply(learners, function(lrn) {
    lapply(tune.controls, function(tune.ctrl) {
      makeTuneWrapper(learner = lrn, resampling = rsi, measures = measures, par.set = par.sets[[lrn$id]], control = tune.ctrl, show.info = show.info)
    })
  })
}

giveMeResampleDesc = function(x = 1, ...) {
  rdesc.list = list(
    subsample = makeResampleDesc("Subsample", iters = 2, ...),
    cv = makeResampleDesc("CV", iters = 10, ...),
    holdout = makeResampleDesc("Holdout", ...),
    inner = makeResampleDesc("Holdout")
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
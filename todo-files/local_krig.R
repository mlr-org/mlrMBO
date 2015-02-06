makeRLearner.regr.kmlocal = function() {
  makeRLearnerRegr(
    cl = "regr.kmlocal",
    package = "DiceKriging",
    par.set = makeParamSet(
      makeIntegerLearnerParam("min.clust.size", lower = 1L, default = 3L),
      # our own params
      makeNumericLearnerParam("clust.cor.th", lower = 0, upper = 1, default = 0.99),
      # km param
      makeDiscreteLearnerParam(id = "covtype", default = "matern5_2",
        values = list("gauss", "matern5_2", "matern3_2", "exp", "powexp")),
      makeNumericVectorLearnerParam(id = "noise.var"),
      makeDiscreteLearnerParam(id = "optim.method", default = "BFGS", values = list("BFGS", "gen")),
      makeNumericVectorLearnerParam(id = "lower"),
      makeNumericVectorLearnerParam(id = "upper"),
      makeUntypedLearnerParam(id = "control")    
    ),
    properties = c("numerics", "se"),
    name = "Kriging-Local",
    short.name = "kmloc",
    note = ""
  )
}

trainLearner.regr.kmlocal = function(.learner, .task, .subset, min.clust.size = 3L, clust.cor.th = 0.99, ...) {

  d = getTaskData(.task, .subset, target.extra = TRUE)
  x = as.matrix(d$data)
  y = d$target
  local.x = x
  local.y = y
  local.models = list()
  local.centers = list()
  local.centers.i = numeric(0L)
  local.i = seq_row(x)
  
  # fit gloabl model to all points
  # FIXME: what about passing hyperpars to km? and "...". also note we do further km() calls below!
  
  suppressAll({
  globm = DiceKriging::km(design = x, response = y)
  })
  
  iter = 1L
  
  while (TRUE) {
    # select best y
    min.j = getMinIndex(local.y, ties.method = "random")
    min.x = local.x[min.j, , drop = FALSE]
    min.y = local.y[min.j]
    
    # get correlation of all points to best and find out which are highly correlated
    cov.x.minx = covMat1Mat2(globm@covariance, local.x, min.x)[,1L] / globm@covariance@sd2
    iscor = cov.x.minx >= clust.cor.th
    cluster = which(iscor)
  
    # if we have too few points in cluster, break
    if (length(cluster) < min.clust.size) 
      break
    
    # fit submodel to minx and highly correlated points
    suppressAll({
      locm = DiceKriging::km(design = local.x[cluster, , drop = FALSE], response = local.y[cluster])
    })
    local.models[[iter]] = locm
    local.centers[[iter]] = min.x
    local.centers.i[[iter]] = local.i[min.j]
    
    # remove best and highly correlated points from set, and handle rest points
    local.x = local.x[!iscor, , drop = FALSE]
    local.y = local.y[!iscor]
    local.i = local.i[!iscor]
      
    iter = iter + 1L
  }
  
  local.centers = do.call(rbind, local.centers)
  return(list(global.model = globm, local.models = local.models, local.centers = local.centers, 
    local.centers.i = local.centers.i))
}

predictLearner.regr.kmlocal = function(.learner, .model, .newdata, ...) {
  # some shortcuts
  se = (.learner$predict.type != "response")
  lm = .model$learner.model
  gm = lm$global.model
  locms = lm$local.models
  loccs = lm$local.centers
  k = length(locms)
  nd = as.matrix(.newdata)
  # result
  res = matrix(NA, nrow(.newdata), 2L)
  colnames(res) = c("mean", "se")

  # predict sel points with mod, then store in res
  predAndStore = function(mod, sel) {
    if (any(sel)) {
      nd2 = .newdata[sel, , drop = FALSE]
      p = predict(mod, newdata = nd2, type = "SK", se.compute = se)
      res[sel, 1L] <<- p$mean     
      if (se)
        res[sel, 2L] <<- p$sd
    }
  }
  
  if (k > 0L) {
    covs = covMat1Mat2(gm@covariance, loccs, nd) / gm@covariance@sd2
    
    # loop thru local models, start from "finest", 
    # if newdata points are correlated to model (and not predicted before) predict them
    for (i in 1:k) {
      covs2 = covs[i,]   
      sel = (covs2 >= .learner$par.vals$clust.cor.th) & is.na(res[, 1L])
      predAndStore(locms[[i]], sel)
    }
  }
  
  # handle all remaining points with global model
  sel = is.na(res[, 1L])
  predAndStore(gm, sel)
  
  if (se)
    return(res)
  else
    return(res[, 1L])
}

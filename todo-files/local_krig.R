makeRLearner.regr.kmlocal = function() {
  makeRLearnerRegr(
    cl = "regr.kmlocal",
    package = "DiceKriging",
    par.set = makeParamSet(
      makeIntegerLearnerParam("min.clust.size", lower = 3L, default = 4L),
      # our own params
      makeNumericLearnerParam("clust.cor.th", lower = 0, upper = 1, default = 0.7),
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

trainLearner.regr.kmlocal = function(.learner, .task, .subset, min.clust.size = 4L, clust.cor.th = 0.7, ...) {

  d = getTaskData(.task, .subset, target.extra = TRUE)
  x = as.matrix(d$data)
  y = d$target
  
  # fit global model to all points
  # FIXME: what about passing hyperpars to km? and "...". also note we do further km() calls below!
  
  suppressAll({
    globm = DiceKriging::km(design = x, response = y)
  })
  
  lcount = 0L  
  local.x = x
  local.y = y

  local.models = list()
  local.centers = list()
  local.centers.minc = list()    
  
  while (TRUE) {
    # select best y
    min.i = getMinIndex(local.y, ties.method = "random")
    min.x = local.x[min.i, , drop = FALSE]
    min.y = local.y[min.i]
    
    # get correlation of all points to best and find out which are highly correlated
    cov.x.minx = covMat1Mat2(globm@covariance, local.x, min.x)[,1L] / globm@covariance@sd2
    cov.x.minx = sort(cov.x.minx, decreasing = TRUE, index.return = TRUE)
    cov.x.minx.delta = head(cov.x.minx$x, n = length(cov.x.minx$x)-1) - tail(cov.x.minx$x, n = length(cov.x.minx$x)-1)
    cov.x.minx.delta = scale(cov.x.minx.delta)
    clusters = kmeans(cov.x.minx$x, cov.x.minx$x[cov.x.minx.delta>0], iter.max = length(cov.x.minx)+1, nstart = 1)
    k = 1
    ic = sum(clusters$cluster==k)    
    while(ic < min.clust.size) {
      k = k + 1
      ic = ic + sum(clusters$cluster==k)
    }
    while(cov.x.minx$x[ic] >= clust.cor.th) {
      lcount = lcount + 1
      # fit submodel to minx and highly correlated points
      suppressAll({
        locm = DiceKriging::km(design = local.x[cov.x.minx$ix[1:ic], , drop = FALSE], response = local.y[cov.x.minx$ix[1:ic]])
      })
      local.models[[lcount]] = locm
      local.centers[[lcount]] = min.x
      local.centers.minc[[lcount]] = cov.x.minx$x[ic]
      ###############################################
      # insert mean optimization on local model     #
      # box constraints are min, max of the cluster #
      # add new point to point set                  #
      ###############################################
      # remove best and highly correlated points from set, and handle rest points
      local.x = local.x[ic+1:dim(local.x)[1], , drop = FALSE]
      if(exists("rem.x", mode="numeric")) {
        rem.x = rbind(rem.x, min.x)
      } else {
        rem.x = min.x      
      }      
      local.y = local.y[ic+1:length(local.y)]
      if(exists("rem.y", mode="numeric")) {
        rem.y = c(rem.y, min.y)
      } else {
        rem.y = min.y      
      }
      # select best y
      min.i = getMinIndex(local.y, ties.method = "random")
      min.x = local.x[min.i, , drop = FALSE]
      min.y = local.y[min.i]
      
      # get correlation of all points to best and find out which are highly correlated
      cov.x.minx = covMat1Mat2(globm@covariance, local.x, min.x)[,1L] / globm@covariance@sd2
      cov.x.minx = sort(cov.x.minx, decreasing = TRUE, index.return = TRUE)
      cov.x.minx.delta = head(cov.x.minx$x, n = length(cov.x.minx$x)-1) - tail(cov.x.minx$x, n = length(cov.x.minx$x)-1)
      cov.x.minx.delta = scale(cov.x.minx.delta)
      clusters = kmeans(cov.x.minx$x, cov.x.minx$x[cov.x.minx.delta>0], iter.max = length(cov.xminx)+1, nstart = 1)
      k = 1
      ic = sum(clusters$cluster==k)
      while(ic < min.clust.size) {
        k = k + 1
        ic = ic + sum(clusters$cluster==k)
      }
    }
  }
  if(exists("rem.x", mode="numeric")) {
    rem.x = rbind(rem.x, local.x)
  } else {
    rem.x = local.x      
  }        
  if(exists("rem.y", mode="numeric")) {
    rem.y = c(rem.y, local.y)
  } else {
    rem.y = local.y      
  }  
  
  local.centers = do.call(rbind, local.centers)
  local.centers.minc = do.call(c, local.centers.minc)
  return(list(global.model = globm, local.models = local.models, local.centers = local.centers, 
    local.centers.minc = local.centers.minc))
}

predictLearner.regr.kmlocal = function(.learner, .model, .newdata, ...) {
  # some shortcuts
  se = (.learner$predict.type != "response")
  lm = .model$learner.model
  gm = lm$global.model
  locms = lm$local.models
  loccs = lm$local.centers
  locmincs = lm$local.centers.minc
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
      sel = (covs2 >= locmincs) & is.na(res[, 1L])
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

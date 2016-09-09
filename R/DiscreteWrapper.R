#' @title Treats a categorical variable as a seperator
#'
#' @description
#' This wrapper will make sure that the learner sees numerical values as independent in combination with each categorical variable
#'
#' @param learner [\code{Learner}]\cr
#'   mlr Learner
#' @param aw.quantiles [\code{numeric}]\cr
#'   Quantiles that should be calculated for imputation.
#'   Default is \code{c(0.25,0.5, 0.75)}.
#' @return Wrapped Learner
#' @family wrapper
#' 
#' 

makeDiscreteWrapper = function(learner) {

  myTrafo = function(data, target, category, na.val) {
  catf("run mytrafon on data with")
    
    data = cbind(dcast(data.notarget, as.formula(paste0("seq_along(", category, ")~",category)), fill = na.val, value.var = "x", drop = FALSE)[, -1, drop = FALSE], data[,colnames(data) %in% c(category, target), drop = FALSE])
    return(data)
  }

  trainfun = function(data, target, args) {
    data.notarget = data[, colnames(data) != target, drop = FALSE]
    data.notarget = cbind(dcast(data.notarget, as.formula(paste0("seq_along(", category, ")~",category)), fill = na.val, value.var = "x", drop = FALSE)[, -1, drop = FALSE], data[,colnames(data) %in% c(category, target), drop = FALSE])
  }

  wrpTrain = function(data, target, args) {
    data = myTrafo(data = data, target = target, category = args$category, na.val = args$na.val)
    list(data = data, control = list())
  }

  wrpPredict = function(data, target, args, control) {
    myTrafo(data = data, target = target, category = args$category, na.val = args$na.val)
  }

  lrn = makePreprocWrapper(learner = learner, train = wrpTrain, predict = wrpPredict, par.vals = list(category = "category", na.val = 1024))
  addClasses(lrn, "DiscreteWrapper")
}
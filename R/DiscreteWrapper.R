#' @title Treats a categorical variable as a seperator
#'
#' @description
#' This wrapper will make sure that the learner sees numerical values as independent in combination with each categorical variable
#'
#' @param learner [\code{Learner}]\cr
#'   mlr Learner
#' @param discw.category [\code{charachter}]\cr
#'   names of the columns that indicate the categorical value to split before training.
#'   If value is NULL (default) then the first categorical value will be taken into account.
#' @return Wrapped Learner
#' @family wrapper
#' 
#' @export

makeDiscreteWrapper = function(learner, discw.category = NULL, discw.na.val = NULL) {

  args = list(
    discw.category = discw.category,
    discw.na.val = discw.na.val)

  rm(list = names(args))

  trainfun = function(data, target, args) {
    data.notarget = data[, colnames(data) != target, drop = FALSE]
    discw.category = coalesce(
      args$discw.category,
      names(which(sapply(data.notarget, is.factor)))[1])
    discw.na.val = coalesce(
      args$discw.na.val,
      max(as.matrix(data.notarget[, sapply(data.notarget, is.numeric)])) * 10
      )
    data = discreteWrapperTrafoHelper(data = data, target = target, category = discw.category, na.val = args$na.val)
    list(data = data, control = list(discw.category = discw.category, discw.na.val = discw.na.val))
  }

  predictfun = function(data, target, args, control) {
    discreteWrapperTrafoHelper(data = data, target = target, category = control$discw.category, na.val = control$discw.na.val)
  }

  lrn = makePreprocWrapper(learner = learner, train = trainfun, predict = predictfun, par.vals = args)
  addClasses(lrn, "DiscreteWrapper")
}

discreteWrapperTrafoHelper = function(data, target, category, na.val) {
  data.notarget = data[, colnames(data) != target, drop = FALSE]
  cbind(
    reshape2::dcast(data.notarget, as.formula(paste0("seq_along(", category, ")~",category)), fill = na.val, value.var = "x", drop = FALSE)[, -1, drop = FALSE], 
    data[,colnames(data) %in% c(category, target), drop = FALSE])
}
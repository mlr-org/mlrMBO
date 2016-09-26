#' @title Treats a categorical variable as a seperator
#'
#' @description
#' This wrapper will make sure that the learner sees numerical values as independent in combination with each categorical variable
#'
#' @param learner [\code{Learner}]\cr
#'   mlr Learner
#' @param discw.category [\code{charachter(1)}]\cr
#'   name of the column that indicates the categorical value to split before training.
#'   If value is NULL (default) then the first categorical value will be taken into account.
#' @param discw.na.val [\code{numeric(1)}]\cr
#'   Number that structural missing values should be imputed with.
#' @return Wrapped Learner
#' @family wrapper
#' 
#' @export

makeDiscreteWrapper = function(learner, discw.category = NULL, discw.na.val = NULL) {

  assertCharacter(discw.category, null.ok = TRUE)
  assertNumber(discw.na.val, na.ok = TRUE, null.ok = TRUE)

  args = list(
    discw.category = discw.category,
    discw.na.val = discw.na.val)

  rm(list = names(args))

  trainfun = function(data, target, args) {
    discw.category = coalesce(
      args$discw.category,
      names(which(sapply(data.notarget, is.factor)))[1])
    discw.na.val = coalesce(
      args$discw.na.val,
      max(as.matrix(data.notarget[, sapply(data.notarget, is.numeric)])) * 10
      )
    data = discreteWrapperTrafoHelper(data = data, target = target, category = discw.category, na.val = discw.na.val)
    list(data = data$data, control = list(discw.category = discw.category, discw.na.val = discw.na.val, categories = data$categories))
  }

  predictfun = function(data, target, args, control) {
    data = discreteWrapperTrafoHelper(data = data, target = target, category = control$discw.category, na.val = control$discw.na.val, categories = control$categories)
    data$data
  }

  lrn = makePreprocWrapper(learner = learner, train = trainfun, predict = predictfun, par.vals = args)
  addClasses(lrn, "DiscreteWrapper")
}

discreteWrapperTrafoHelper = function(data, target, category, na.val, categories = NULL) {
  cont.vars = setdiff(colnames(data), c(category, target))
  categories = coalesce(categories, unique(data[[category]]))
  res = lapply(categories, function(x) {
    this.data = data[, cont.vars, drop = FALSE]
    this.data[data[[category]] != x,] = na.val
    setNames(this.data, paste0(x,".",colnames(this.data)))
  })
  res = do.call(cbind, c(list(data[,colnames(data) %in% c(category, target), drop = FALSE]), res))
  list(data = res, categories = categories)
}

# Chooses the final point according to heuristic.
#
# @param fun [\code{function(x, ...)}}]\cr
#   Fitness function to optimize.
# @param par.set [\code{param.set}]\cr
#   Parameter set.
# @param model [\code{\link[mlr]{Learner}}]\cr
#   Fitted surrogate model.
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param y.name [\code{character}]\cr
#   Target function name.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{integer(1)}] Index of the final point.
chooseFinalPoint = function(fun, par.set, model, opt.path, y.name, control) {
  df = as.data.frame(opt.path, discretes.as.factor = TRUE)
  input.names = setdiff(colnames(df), c(y.name, "dob", "eol"))
  switch (control$final.method,
    "last.proposed" = nrow(df),
    "best.true.y" = getOptPathBestIndex(opt.path, ties = "random"),
    "best.predicted" = which(rank(ifelse(control$minimize, 1, -1) * 
      predict(model, newdata = df[, input.names, drop = FALSE])$data$response, ties.method = "random") == 1L))
}

# Chooses the final point according to heuristic.
#
# @param fun [\code{function(x, ...)}}]\cr
#   Fitness function to optimize.
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param model [\code{\link[mlr]{Learner}}]\cr
#   Fitted surrogate model.
# @param task [\code{\link[mlr]{SupervisedTask}}]\cr
#   Fitted surrogate model.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{integer(1)}] Index of the final point.
chooseFinalPoint = function(opt.state) {
  opt.problem =  getOptStateOptProblem(opt.state)
  opt.path = getOptStateOptPath(opt.state)
  control = getOptProblemControl(opt.problem)
  if (control$final.method == "last.proposed") {
    getOptPathLength(opt.path)
  } else if (control$final.method == "best.true.y") {
    getOptPathBestIndex(opt.path, ties = "random")
  } else if (control$final.method == "best.predicted") {
    maximize.mult = ifelse(control$minimize, 1, -1)
    model = getOptStateModels(opt.state)$models[[1L]]
    task = getOptStateTasks(opt.state, predictive = TRUE)[[1]]
    pred = predict(model, task = task)
    which(rank(maximize.mult * pred$data$response, ties.method = "random") == 1L)
  }
}

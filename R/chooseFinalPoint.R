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
  chooseFinalPoint2(control, opt.path, getOptStateModels(opt.state)$models[[1L]], getOptStateTasks(opt.state)[[1]])
}

# we need this helper function for plotting where we don't have a opt.state (yet?)
chooseFinalPoint2 = function(control, opt.path, model, task) {
  switch (control$final.method,
          "last.proposed" = getOptPathLength(opt.path),
          "best.true.y" = getOptPathBestIndex(opt.path, ties = "random"),
          "best.predicted" = which(rank(ifelse(control$minimize, 1, -1) *
                                          predict(model, task = task)$data$response, ties.method = "random") == 1L))
}

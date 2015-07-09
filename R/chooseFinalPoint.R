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
chooseFinalPoint = function(tuningState) {
  tuningProblem =  getTuningStateTuningProblem(tuningState)
  opt.path = getTuningStateOptPath(tuningState)
  control = getTuningProblemControl(tuningProblem)
  switch (control$final.method,
    "last.proposed" = getOptPathLength(opt.path),
    "best.true.y" = getOptPathBestIndex(opt.path, ties = "random"),
    "best.predicted" = which(rank(ifelse(control$minimize, 1, -1) * 
      predict(getTuningStateModels(tuningState)$models[[1L]], task = getTuningStateTasks(tuningState)[[1]])$data$response, ties.method = "random") == 1L))
}

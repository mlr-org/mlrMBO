# Generates multi objective classification tasks
# Wen want to classify points into dominated and non-dominated points
#
# - only uses x and y columns of optpath
# - converts data types for classif model
# - imputes features
#
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{\link[mlr]{SupervisedTask}}]
makeTasksMultiCritClassif = function(opt.path, control) {
  
  data = convertOptPathToDf(opt.path, control, include.y = FALSE)
  Y = getOptPathY(opt.path)
  
  if (control$multicrit.method == "epo" && control$multicrit.epo.type == "regr") {
    rank = nds_rank(t(Y))
    data = cbind(data, rank = rank)
    task = makeRegrTask(target = "rank", data = data)
    return(task)
  }
  
  
  is.nondom = !is_dominated(t(Y))
  
  #is.nondom = !sapply(seq_row(Y), function(i) any(apply(Y[-i, ], 1, function(vec) all(Y[i, ] + 0.01 > vec))))
  
  data = cbind(data, is.nondom = is.nondom)
  
  task = makeClassifTask(target = "is.nondom", data = data, positive = "TRUE")
  task
}

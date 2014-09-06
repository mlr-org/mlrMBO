# create result object for MCO#
# get indices of pareto front from path, then add rest
makeMBOMultiCritResult = function(opt.path, convergence, models) {
  inds = getOptPathParetoFront(opt.path, index = TRUE)
  res = makeS3Obj(c("MBOMultiObjResult", "MBOResult"),
    pareto.front = getOptPathY(opt.path)[inds, , drop = FALSE],
    pareto.set = lapply(inds, function(i) getOptPathEl(opt.path, i)$x),
    pareto.inds = inds,
    opt.path = opt.path,
    convergence = convergence,
    models = models
  )
}

#' @export
print.MBOMultiObjResult = function(x, ...) {
  print(x$pareto.front)
  print(tail(as.data.frame(x$opt.path), 10))
}



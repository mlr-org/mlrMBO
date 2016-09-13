# Takes x.vals and y.vals from the opt.path, possibly imputes missing x.vals and
# returns a data.frame.
#
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{data.frame}]
convertOptPathToDf = function(opt.path, control) {
  df = as.data.frame(opt.path, include.rest = FALSE)
  df = convertDataFrameCols(df, ints.as.num = TRUE, logicals.as.factor = TRUE)
  return(df)
}


# Build the aggregation aggr over repeated noisy evaluations
# returns a list with data.frame (of means) like above + n.eval - numeric vector
# giving the number of reevaluations per point
convertOptPathToMeanDf = function(opt.path, control, aggr = mean) {
  
  design = getOptPathX(opt.path)
  design = convertDataFrameCols(design, ints.as.num = TRUE, logicals.as.factor = TRUE)
  
  # Get mean performances for each point - each point can have multiple evaluations
  unique.design = unique(convertRowsToList(design))
  Y.mean = lapply(unique.design, function(tmp) {
    reevals = sapply(seq_row(design), function(i) all(tmp == design[i, ], na.rm = TRUE))
    Ys = getOptPathY(opt.path, drop = FALSE)[reevals, , drop = FALSE]
    list(perf = apply(Ys, 2, aggr), n.evals = sum(reevals))
  })
  
  design = convertListOfRowsToDataFrame(unique.design, col.names = names(design))
  
  Ys = extractSubList(Y.mean, "perf")
  if (is.matrix(Ys)) {
    design = cbind(design, t(Ys))
  } else {
    design[[control$y.name]] = Ys
  }
  n.evals = extractSubList(Y.mean, "n.evals")
  
  return(list(df = design, n.evals = n.evals))
}

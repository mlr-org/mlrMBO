# Takes x.vals and y.vals from the opt.path, possibly imputes missing x.vals and
# returns a data.frame
convertOptPathToDf = function(par.set, opt.path, control, impute.feats) {
  pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  df = as.data.frame(opt.path, discretes.as.factor = TRUE)[, c(pids, control$y.name)]
  df = convertDataFrameCols(df, ints.as.num = TRUE, logicals.as.factor = TRUE)
  # FIXME: Use mlr here!
  if (impute.feats)
    df = imputeFeatures(df, par.set, control)
  return(df)
}

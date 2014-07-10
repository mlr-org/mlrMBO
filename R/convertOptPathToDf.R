# Takes x.vals and y.vals from the opt.path, possibly imputes missing x.vals and
# returns a data.frame
convertOptPathToDf = function(par.set, opt.path, control) {
  df = as.data.frame(opt.path, discretes.as.factor = TRUE, include.rest = FALSE)
  df = convertDataFrameCols(df, ints.as.num = TRUE, logicals.as.factor = TRUE)
  return(df)
}

# for numeric columns:
#   method = up: NAs are replaced with the 2 * upper bound
#   method = median: NAs are replaced with median and extra col is added where imputation was done
# for factor columns:
#   NAs are coded as a new level
imputeFeatures = function(data, par.set, control) {
  cnd = colnames(data)
  upp = 2 * getUpper(par.set, with.nr=TRUE)
  pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  as.data.frame(sapply(colnames(data), simplify=FALSE, function(pid) {
    x = data[[pid]]
    # do not handle numeric y column, only inputs
    if (is.numeric(x) && pid %in% pids) {
      inds = is.na(x)
      if(control$feature.impute == "up")
        return(replace(x, inds, upp[[pid]]))
      else if(control$feature.impute == "median"){
        return(data.frame(vals=replace(x, inds, median(x, na.rm=TRUE)), nas=as.factor(inds)))
      }
    } else if (is.factor(x)) {
      inds = is.na(x)
      if (any(inds)) {
        levels(x) = c(levels(x), "__miss__")
        return(replace(x, inds, "__miss__"))
      }
    }
    return(x)
  }), stringsAsFactors=FALSE)
}

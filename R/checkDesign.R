# Check if the initial design is large enough. I.e.:
# Every factor level is "visited" for factors
# and every dependent feature has NAs and not-NAs
checkInitDesign = function(design, par.set) {
  # discrete Params: all levels are visited
  disc.params = filterParams(par.set, type = "discrete")
  par.ids = getParamIds(disc.params)
  disc.params.values = lapply(getValues(disc.params), unlist, use.names = FALSE)
  not.visited = Map(setdiff, disc.params.values[par.ids], design[par.ids])
  if (any(lengths(not.visited) > 0L))
    return(FALSE)

  # if the param has requires, check if NA and normal level is visited
  has.requires = vlapply(par.set$pars, hasRequires)
  sum.na = lapply(design[has.requires], function(x) sum(is.na(x)))
  if (any(sum.na == 0 | sum.na == nrow(design)))
    return(FALSE)

  return(TRUE)
}

# Check if a line of the new design is able to predict atm
# corresponding to the current design
checkPredictionData = function(new.design, design) {
  ok = rep(TRUE, (nrow(new.design)))
  for (i in seq_col(new.design)) {
    # check factor level is visited
    if (is.factor(new.design[, i]))
      ok = ok & new.design[, i] %in% design[, i]
    feat.nas = is.na(new.design[, i])
    design.nas = is.na(design[, i])
    ok = ok & ifelse(feat.nas, any(design.nas), any(!design.nas))
  }
  return(ok)
}

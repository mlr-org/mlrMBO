
# Check if the initial design is large enoug. in detail:
# every factor level is "visited" for factors
# and every dependet feature has NAs and not-NAs

checkInitDesign = function(design, par.set) {
  # discrete Params
  # all levels are visited
  disc.params = filterParams(par.set, "discrete")
  disc.params.values = getValues(disc.params)
  disc.design = design[getParamIds(disc.params)]
  disc.design.values = apply(disc.design, 2, unique)
  disc.ok = sapply(seq_along(disc.params.values), function(i)
    all(disc.params.values[[i]] %in% disc.design.values[[i]]))
  
  # if the param has requires, check if NA and normal level is visited
  has.requires = sapply(par.set$pars, hasRequires)
  requires.design = design[, has.requires]
  requires.ok = apply(is.na(requires.design), 2, function(x) any(x) && any(!x))
  
  return(all(disc.ok, requires.ok))
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

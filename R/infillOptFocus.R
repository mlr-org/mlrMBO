# Random search, where we shrink the region of interest after restarts
# around the currently best point. only numeric / ints are currently "shrunken"
# works for ALL parameter sets
#
#FIXME it would be nice to have a REASONABLE way to shrink categorical stuff too.
#FIXME should we shrink if a local value is NA (dependent param)
#
# See infillOptCMAES.R for interface explanation.
infillOptFocus = function(infill.crit, model, control, par.set, opt.path, design, iter, ...) {
  global.y = Inf

  # restart the whole crap some times
  for (restart.iter in 1:control$infill.opt.restarts) {
    # do iterations where we focus the region-of-interest around the current best point
    for (local.iter in 1:control$infill.opt.focussearch.maxit) {
      # predict on design where NAs were imputed, but return proposed points with NAs
      newdesign = generateDesign(control$infill.opt.focussearch.points, par.set,
        randomLHS)
      y = infill.crit(newdesign, model, control, par.set, design, iter, ...)
      # get current best value
      local.index = getMinIndex(y, ties.method = "random")
      local.y = y[local.index]
      local.x.df = newdesign[local.index, , drop = FALSE]
      local.x.list = dfRowToList(recodeTypes(local.x.df, par.set), par.set, 1)

      # if we found a new best value, store it
      if (local.y < global.y) {
        global.x.df = local.x.df
        global.y = local.y
      }

      # now shrink par.set object so we search more locally
      for (i in seq_along(par.set$pars)) {
        par = par.set$pars[[i]]
        val = local.x.list[[par$id]]
        # only shrink when there is a value
        if (!isScalarNA(val)) {
          if (isNumeric(par)) {
            # shrink to range / 2, centered at val
            range = par$upper - par$lower
            par$lower = pmax(par$lower, val - (range / 4))
            par$upper = pmin(par$upper, val + (range / 4))
            if (isInteger(par)) {
              par$lower = floor(par$lower)
              par$upper = ceiling(par$upper)
            }
          } else if (isDiscrete(par)) {
            # randomly drop a level, which is not val
            if (length(par$values) > 1L) {
              val.names = names(par$values)
              # remove current val from delete options, should work also for NA
              val.names = setdiff(val.names, val)
              to.del = sample(seq_along(val.names), 1)
              par$values = par$values[-i]
            }
          }
        }
        par.set$pars[[i]] = par
      }
    }
  }
  recodeTypes(global.x.df, par.set)
}

# as we operate on other types for the learner (ints are nums, logs are factors),
# we have to convert back sometimes for dfRowsToList to work
recodeTypes = function(df, par.set) {
  types = unlist(lapply(par.set$pars, function(p) rep(p$type, p$len)))
  for (i in seq_col(df)) {
    if (types[i] %in% c("integer", "integervector"))
      df[, i] = as.integer(df[,i])
    else if (types[i] %in% c("logical", "logicalvector"))
      df[, i] = as.logical(as.character(df[,i]))
  }
  return(df)
}

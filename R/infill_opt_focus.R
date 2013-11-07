
# random search, where we shrink the region of interest after restarts
# around the currently best point. only numeric / ints are currently "shrunken"
# works for ALL parameter sets

#FIXME it would be nice to have a REASONABLE way to shrink categorical stuff too.
#FIXME should we shrink if a local value is NA (dependent param)

infillOptFocus = function(infill.crit, model, control, par.set, opt.path, design) {
  global.x = NULL
  global.y = Inf

  # restart the whole crap some times
  for (restart.iter in 1:control$infill.opt.restarts) {
    # do iterations where we focus the region-of-interest around the current best point
    for (local.iter in 1:control$infill.opt.random.maxit) {
      # predict on design where NAs were imputed, but return propsed points with NAs
      newdesign = generateDesign(control$infill.opt.random.points, par.set,
        randomLHS, ints.as.num=TRUE, logicals.as.factor=TRUE)
      y = infill.crit(refactorNAs(newdesign, par.set), model, control, par.set, design)
      # get current best value
      local.index = getMinIndex(y, ties.method="random")
      local.y = y[local.index]
      local.x = newdesign[local.index, , drop=FALSE]
      # if we found a new best value, store it
      if (local.y < global.y) {
        global.x = local.x
        global.y = local.y
      }

      # now shrink par.set object so we search more locally
      for (i in seq_along(par.set$pars)) {
        par = par.set$pars[[i]]
        val = local.x[[i]]
        if (par$type %in% c("numeric", "integer", "numericvector", "integervector")) {
          # shrink to range / 2, centered at val
          range = par$upper - par$lower
          par$lower = pmax(par$lower, val - range/4)
          par$upper = pmin(par$upper, val + range/4)
          if (par$type %in% c("integer", "integervector")) {
            par$lower = floor(par$lower)
            par$upper = ceiling(par$upper)
          }
        } else if (par$type %in% c("discrete", "discretevector")) {
          # randomly drop a level, which is not val
          if (length(par$values) > 1L) {
            val.names = names(par$values)
            # remove current val from delete options, should work also for NA
            val.names = setdiff(val.names, val)
            to.del = sample(seq_along(val.names), 1)
            par$values = par$values[-i]
          }
        }
        par.set$pars[[i]] = par
      }
    }
  }
  # convert back to correct type
  types = unlist(lapply(par.set$pars, function(p) rep(p$type, p$len)))
  for (i in seq_col(global.x)) {
    if (types[i] %in% c("integer", "integervector"))
      global.x[,i] = as.integer(global.x[,i])
    else if (types[i] %in% c("logical", "logicalvector"))
      global.x[,i] = as.logical(as.character(global.x[,i]))
  }
  global.x
}


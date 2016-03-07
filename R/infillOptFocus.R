# Random search, where we shrink the region of interest after restarts
# around the currently best point. only numeric / ints are currently "shrunken"
# works for ALL parameter sets
#
#FIXME it would be nice to have a REASONABLE way to shrink categorical stuff too.
#FIXME should we shrink if a local value is NA (dependent param)
#
# See infillOptCMAES.R for interface explanation.
infillOptFocus = function(infill.crit, models, control, par.set, opt.path, design, iter, ...) {
  global.y = Inf
  
  allRequirements = extractSubList(par.set$pars, "requires", simplify = FALSE)
  allUsedVars = unique(do.call(base::c, sapply(allRequirements, all.vars)))
  forbiddenVars = getParamIds(filterParams(par.set, type = c("discretevector", "logicalvector")))
  if (any(allUsedVars%in% forbiddenVars)) {
    stop("Cannot do focus search when some variables have requirements that depend on discrete or logical vector parameters.")
  }
  

  # perform multiple starts
  for (restart.iter in seq_len(control$infill.opt.restarts)) {
    # copy parset so we can shrink it
    ps.local = par.set
    
    # handle discrete vectors:
    # The problem is that for discrete vectors, we can't adjust the values dimension-wise.
    # Therefore, for discrete vectors, we always drop the last level and instead have a
    # mapping that maps, for each discrete vector param and for each dimension, from 
    # the sampled value (levels 1 to n - #(dropped levels)) to levels with random dropouts.
    discreteVectorMapping = lapply(filterParams(par.set, type = c("discretevector", "logicalvector"))$pars,
        function(param) rep(list(setNames(names(param$values), names(param$values))), param$len))
    discreteVectorMapping = do.call(base::c, discreteVectorMapping)
    # the resulting object is NULL if there are no discrete / logical vector params

    if (!is.null(discreteVectorMapping)) {
      mappedPars = filterParams(par.set, type = c("discretevector", "logicalvector"))
      names(discreteVectorMapping) = getParamIds(mappedPars, with.nr = TRUE, repeated = TRUE)
    }
    

    # do iterations where we focus the region-of-interest around the current best point
    for (local.iter in seq_len(control$infill.opt.focussearch.maxit)) {
      # predict on design where NAs were imputed, but return proposed points with NAs
      newdesign = generateDesign(control$infill.opt.focussearch.points, ps.local, randomLHS)

      # convert to param encoding our model was trained on and can use
      newdesign = convertDataFrameCols(newdesign, ints.as.num = TRUE, logicals.as.factor = TRUE)

      # handle discrete vectors
      for (dfindex in names(discreteVectorMapping)) {
        mapping = discreteVectorMapping[[dfindex]]
        levels(newdesign[[dfindex]]) = mapping[levels(newdesign[[dfindex]])]
      }

      y = infill.crit(newdesign, models, control, par.set, design, iter, ...)

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

      # now shrink ps.local object so we search more locally
       ps.local$pars = lapply(ps.local$pars, function(par) {
         # only shrink when there is a value
         val = local.x.list[[par$id]]
         if (isScalarNA(val)) {
           return(par)
         }
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
           if (length(par$values) <= 1L) {
             return(par)
           }
           # need to do some magic to handle discrete vectors
           if (par$type %nin% c("discretevector", "logicalvector")) {
             val.names = names(par$values)
             # remove current val from delete options, should work also for NA
             val.names = val.names[!sapply(par$values, identical, y=val)]  # remember, 'val' can be any type
             to.del = sample(val.names, 1)
             par$values[[to.del]] = NULL
           } else {
             # we remove the last element of par$values and a random element for
             # each dimension in discreteVectorMapping.
             par$values = par$values[-length(par$values)]
             if (par$type != "logicalvector") {
               # for discretevectorparam val would be a list; convert to character vector
               val = names(val)
             }
             for (dimnum in seq_len(par$len)) {
               dfindex = paste0(par$id, dimnum)
               newmap = val.names = discreteVectorMapping[[dfindex]]
               val.names = val.names[val.names != val[dimnum]]
               to.del = sample(val.names, 1)
               newmap = newmap[newmap != to.del]
               names(newmap) = names(par$values)
               discreteVectorMapping[[dfindex]] <<- newmap
             }
           }
         }
         return(par)
      })
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

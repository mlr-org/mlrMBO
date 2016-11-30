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
  
  discrete.vector.pars = filterParams(par.set, type = c("discretevector", "logicalvector"))
  
  if (any(ParamHelpers:::getRequiredParamNames.ParamSet(par.set) %in% getParamIds(discrete.vector.pars)))
    stop("Cannot do focus search when some variables have requirements that depend on discrete or logical vector parameters.")
  

  # perform multiple starts
  for (restart.iter in seq_len(control$infill.opt.restarts)) {
    # copy parset so we can shrink it
    ps.local = par.set
    
    # Handle discrete vectors (and logical vectors):
    # The problem is that for discrete vectors, we can't adjust the range dimension-wise.
    # Instead we store the range of each discrete vectorparameter dimension in the list of named characters
    # `discrete.vector.mapping`. In each iteration a random value (that does not contain
    # the optimum) is dropped from each vector on this list. The $values of the parameters in the parameterset also
    # need to be modified to reflect the reduced range: from them, always the last value is dropped.
    # Then `discrete.vector.mapping` is a mapping that maps, for each discrete vector param dimension
    # with originally n values, from the sampled value (levels 1 to n - #(dropped levels)) to the acutal levels with
    # random dropouts.
    #
    # Since the requirements of the param set are queried while generating the design, this breaks if
    # there are requirements depending on discrete vector parameters.
    discrete.vector.mapping = lapply(discrete.vector.pars$pars,
        function(param) rep(list(setNames(names(param$values), names(param$values))), param$len))
    discrete.vector.mapping = unlist(discrete.vector.mapping, recursive=FALSE)
    if (!isEmpty(discrete.vector.pars)) {
      names(discrete.vector.mapping) = getParamIds(discrete.vector.pars, with.nr = TRUE, repeated = TRUE)
    }
    

    # do iterations where we focus the region-of-interest around the current best point
    for (local.iter in seq_len(control$infill.opt.focussearch.maxit)) {
      # predict on design where NAs were imputed, but return proposed points with NAs
      newdesign = generateDesign(control$infill.opt.focussearch.points, ps.local, randomLHS)

      # convert to param encoding our model was trained on and can use
      newdesign = convertDataFrameCols(newdesign, ints.as.num = TRUE, logicals.as.factor = TRUE)

      # handle discrete vectors
      for (dfindex in names(discrete.vector.mapping)) {
        mapping = discrete.vector.mapping[[dfindex]]
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
           if (length(par$values) == 1L) {
             return(par)
           }
           # need to do some magic to handle discrete vectors
           if (par$type %nin% c("discretevector", "logicalvector")) {
             val.names = names(par$values)
             # remove current val from delete options, should work also for NA
             val.names = val.names[!vlapply(par$values, identical, y=val)]  # remember, 'val' can be any type
             to.del = sample(val.names, 1)
             par$values[[to.del]] = NULL
           } else {
             # we remove the last element of par$values and a random element for
             # each dimension in discrete.vector.mapping.
             par$values = par$values[-length(par$values)]
             if (par$type != "logicalvector") {
               # for discretevectorparam val would be a list; convert to character vector
               val = names(val)
             }
             for (dimnum in seq_len(par$len)) {
               dfindex = paste0(par$id, dimnum)
               newmap = val.names = discrete.vector.mapping[[dfindex]]
               val.names = val.names[val.names != val[dimnum]]
               to.del = sample(val.names, 1)
               newmap = newmap[newmap != to.del]
               names(newmap) = names(par$values)
               discrete.vector.mapping[[dfindex]] <<- newmap
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

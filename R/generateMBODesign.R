# FIXME shorten this. What does this function do

# Generates the initial design for a mbo or parego optimization
#
# @param design [\code{data.frame} | NULL]\cr
#   One of this 3:
#   - Initial design as data frame.
#     If the parameters have corresponding trafo functions,
#     the design must not be transformed before it is passed!
#   - A opt.path object:
#     The design and all saved infos will be extracted from this
#   - \code{NULL}:
#     The design is constructed from the settings in \code{control}.
# @param fun [\code{function(x, ...)}]\cr
#   Fitness function to minimize. The first argument has to be a list of values.
#   The function has to return a single numerical value.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Collection of parameters and their constraints for optimization.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object for mbo.
# @param show.info [\code{logical(1)}]\cr
#   Verbose output on console?
# @param oldopts []\cr
#   Currently set mlr options
# @param more.args []\cr
#   Further arguments for the fitness function.
# @return [\code{list}]:
#  \item{design.x [\code{data.frame}]}{Initial design of x-values.}
#  \item{design.y [\code{dara.frame}]}{Initial design of y-values.}
#  \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
#  \item{times [\code{numeric}]}{Vector of times it took to evaluate the objective.}
generateMBODesign = function(design, fun, par.set, control, show.info, oldopts, more.args = list()) {
  # get parameter ids repeated length-times and appended number
  rep.pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  y.name = control$y.name
  times = numeric(0)
  opt.path2 = initMBOOptPathDF(par.set, control)
  
  # If design is an opt.path, restore it as the new opt.path
  # FIXME use inherits
  # FIXME we want to have a error.messages in EVERY optimization
  if ("OptPath" %in% class(design)) {
    opt.path.restored = TRUE
    opt.path = design
    x.names = extractSubList(par.set$pars, "id")
    design = as.data.frame(opt.path)[, c(x.names, control$y.name)]
  } else {
    opt.path.restored = FALSE
    opt.path = makeOptPathDF(par.set, y.name, control$minimize,
      include.error.message = control$do.impute)
  }
  
  if (is.null(design)) {
    design.x = generateDesign(control$init.design.points, par.set,
      control$init.design.fun, control$init.design.args, trafo = FALSE)
  } else {
    # sanity check: are paramter values and colnames of design consistent?
    if(!setequal(setdiff(colnames(design), y.name), rep.pids))
      stop("Column names of design 'design' must match names of parameters in 'par.set'!")
    
    # sanity check: do not allow transformed designs
    # if no trafo attribute provided we act on the assumption that the design is not transformed
    if ("trafo" %nin% names(attributes(design))) {
      attr(design, "trafo") = FALSE
    } else {
      if (attr(design, "trafo")) {
        stop("Design must not be transformed!")
      }
    } 
    
    design.x = dropNamed(design, y.name)
  }
  # reorder
  design.x = design.x[, rep.pids, drop = FALSE]
  xs = dfRowsToList(design.x, par.set)
  # we now have design.x and its rows as lists in xs

  # compute y-values if missing or initial design generated above
  if (all(y.name %in% colnames(design))) {
    design.y = as.matrix(design[, y.name])
    error.messages = rep("", nrow(design.y))
  } else if (!any(y.name %in% colnames(design))){
    if (show.info)
      messagef("Computing y column for design. Was not provided")
    evals = evalTargetFun(fun, par.set, xs, opt.path, control, show.info, oldopts, more.args)
    design.y = evals$ys
    error.messages = evals$error.messages
    times = c(times, evals$times)
  } else {
    stop("Only part of y-values are provided. Don't know what to do - provide either all or none.")
  }
  
  # add initial values to optimization path
  if (!opt.path.restored) {
    ys = convertRowsToList(design.y)
    if (control$do.impute)
      Map(function(x, y, err) addOptPathEl(opt.path, x = x, y = y, dob = 0, error.message = err),
        xs, ys, evals$error.messages)
    else
      Map(function(x, y) addOptPathEl(opt.path, x = x, y = y, dob = 0), xs, ys)
    Map(function(x, y) addOptPathEl(opt.path2, x = x, y = c(y, 0), dob = 0), xs, ys)
    
  }
  
  return(list(
    design.x = design.x,
    design.y = design.y,
    opt.path = opt.path,
    opt.path2 = opt.path2,
    times = times
    ))
}

initMBOOptPathDF = function(par.set, control) {
  makeOptPathDF(
    par.set,
    y.names = c(control$y.name, control$infill.crit),
    minimize = c(control$minimize, TRUE) # by default we minimize all infill crits
  )
}

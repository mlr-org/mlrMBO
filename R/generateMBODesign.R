# Helper which generates an initial design (if none provided).
#
# If no design is passed, create it. otherwise sanity-check it.
# Either do y-evals or log points to opt.path manually.
#
# @param design [\code{data.frame} | NULL]\cr
#   Initial design as data frame. If none provided one is constructed.
# @param fun [\code{function(x, ...)}]\cr
#   Fitness function to minimize.
# @param par.set [\code{param.set}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @param show.info [\code{logical(1)}]\cr
#   Show info or not?
# @param oldopts [\code{list}]\cr
#   Old options for mlr.
# @param more.args [\code{list}]\cr
#   Further parameters for target function.
# @param extras [\code{list}]\cr
#   List of extra information to be logged in \code{opt.path}.
# @return [\code{NULL}]
generateMBODesign = function(design, fun, par.set, opt.path, control, show.info, oldopts,
  more.args = list(), extras = NULL) {

  # shortcut names
  pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  y.name = control$y.name

  # either create design or check that the provided one seems ok
  if (is.null(design)) {
    design.x = generateDesign(control$init.design.points, par.set,
      fun = control$init.design.fun, fun.args = control$init.design.args, trafo = FALSE)
  } else {
    # sanity check: are paramter values and colnames of design consistent?
    if (!setequal(setdiff(colnames(design), y.name), pids))
      stop("Column names of design 'design' must match names of parameters in 'par.set'!")

    # sanity check: do not allow transformed designs
    # if no trafo attribute provided we act on the assumption that the design is not transformed
    if (!hasAttributes(design, "trafo")) {
      design = setAttribute(design, "trafo", FALSE)
    } else {
      if (attr(design, "trafo")) {
        stop("Design must not be transformed!")
      }
    }

    design.x = dropNamed(design, y.name)
  }
  # reorder + create list of x-points
  design.x = design.x[, pids, drop = FALSE]
  xs = dfRowsToList(design.x, par.set)

  # either only log init design stuff to opt.path or eval y-values
  if (all(y.name %in% colnames(design))) {
    y = as.matrix(design[, y.name, drop = FALSE])
    lapply(seq_along(xs), function(i)
      addOptPathEl(opt.path, x = xs[[i]], y = y[i, ], dob = 0L,
        error.message = NA_character_, exec.time = NA_real_, extra = extras[[i]])
    )
  } else if (all(y.name %nin% colnames(design))) {
    showInfo(show.info, "Computing y column(s) for design. Not provided.")
    evalTargetFun(fun, par.set, 0L, xs, opt.path, control, show.info, oldopts, more.args, extras)
  } else {
    stop("Only part of y-values are provided. Don't know what to do - provide either all or none.")
  }

  invisible(NULL)
}


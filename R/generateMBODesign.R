# Helper which generates an initial design (if none provided).
#
# If no design is passed, create it. otherwise sanity-check it.
# Either do y-evals or log points to opt.path manually.
#
# @param opt.state [\code{OptState} | NULL]\cr
#   Initial Tuning State with empty design slot
# @return [\code{NULL}]

generateMBODesign.OptState = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  extras = getExtras(
    n = getOptProblemInitDesignPoints(opt.problem), 
    prop = NULL, 
    train.time = NA_real_, 
    control = getOptProblemControl(opt.problem)
  )

  design = getOptProblemDesign(opt.problem)
  fun = getOptProblemFun(opt.problem)
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)

  # shortcut names
  pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  y.name = control$y.name

  # either create design or check that the provided one seems ok
  if (is.null(design)) {
    design.x = generateDesign(control$init.design.points, par.set,
      fun = control$init.design.fun, fun.args = control$init.design.args, trafo = FALSE)
    points.diff = control$init.design.points - nrow(design.x)
    if (points.diff > 0L) {
      warningf("Could not generate enough points for init design: Only got %i / %i. Augmenting with %i random points now!",
        nrow(design.x), control$init.design.points, points.diff)
      design.x.rand = generateRandomDesign(points.diff, par.set, trafo = FALSE)
      design.x = rbind(design.x, design.x.rand)
    }
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
      addOptPathEl(getOptStateOptPath(opt.state), x = xs[[i]], y = y[i, ], dob = 0L,
        error.message = NA_character_, exec.time = NA_real_, extra = extras[[i]])
    )
  } else if (all(y.name %nin% colnames(design))) {
    showInfo(getOptProblemShowInfo(opt.problem), "Computing y column(s) for design. Not provided.")
    evalTargetFun.OptState(opt.state, xs, extras)
  } else {
    stop("Only part of y-values are provided. Don't know what to do - provide either all or none.")
  }

}


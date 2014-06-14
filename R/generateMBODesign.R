# generates an initial design
#
# - if no design is passed, create it. otherwise sanity-check it
# - either do y-evals or log points to opt.path manually

generateMBODesign = function(design, fun, par.set, opt.path, control, show.info, oldopts,
  more.args = list(), extras) {

  # shortcut names
  pids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  y.name = control$y.name

  # either create design or check that the provided one seems ok
  if (is.null(design)) {
    design.x = generateDesign(control$init.design.points, par.set,
      control$init.design.fun, control$init.design.args, trafo = FALSE)
  } else {
    # sanity check: are paramter values and colnames of design consistent?
    if (!setequal(setdiff(colnames(design), y.name), pids))
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


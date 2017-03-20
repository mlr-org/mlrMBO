# Function for plotting 1d numeric respectively discrete functions.
# see plotExampleRun for details on each argument

# @param ... [\code{list}]\cr
#   Not used.

# @return [\code{list}] List of ggplot2 objects.
renderExampleRunPlot1d = function(x, iter,
  densregion = TRUE,
  se.factor = 1,
  xlim = NULL, ylim = NULL,
  point.size, line.size,
  trafo = NULL,
  colors = c("red", "blue", "green"), ...)  {
  requirePackages("ggplot2")

  # extract relevant data from MBOExampleRun
  par.set = x$par.set
  names.x = x$names.x
  name.y = x$name.y
  control = x$control
  noisy = control$noisy
  mbo.res = x$mbo.res
  models = mbo.res$models
  models = if (inherits(models, "WrappedModel")) list(models) else models
  names(colors) = c("init", "prop", "seq")

  # check if standard error is available
  se = (x$learner$predict.type == "se")

  propose.points = control$propose.points
  infill.crit.id = getMBOInfillCritId(control$infill.crit)
  # if (control$multifid) {
  #   infill.crit.id = "mfEI"
  #   critfun = infillCritMultiFid.external
  # } else {
    critfun = control$infill.crit$fun
  #}

  # we need to maximize expected improvement
  opt.direction = getMBOInfillCritMultiplier(control$infill.crit)

  # if no iterations provided take the total number of iterations in optimization process
  iter = asInt(iter, lower = 0, upper = length(models))

  if (!is.na(x$global.opt)) {
    global.opt = x$global.opt
  } else {
    global.opt = x$global.opt.estim
  }

  evals = x$evals
  opt.path = mbo.res$opt.path
  evals.x = evals[, getParamIds(opt.path$par.set) , drop = FALSE]

  # helper function for building up data frame of different points
  # i.e., initial design points, infilled points, proposed points for ggplot
  getType = function(x, iter) {
    if (x == 0)
      return("init")
    else if (x > 0 && x < iter)
      return("seq")
    else if (x == iter)
      return("prop")
    else
      return ("future")
  }

  buildPointsData = function(opt.path, iter) {
    type = vcapply(getOptPathDOB(opt.path), getType, iter = iter)
    res = cbind.data.frame(
      convertOptPathToDf(opt.path, control),
      type = type
    )
    res[res$type %nin% "future",]
  }

  plots = list()

  infill.mean = makeMBOInfillCritMeanResponse()$fun
  infill.ei = makeMBOInfillCritEI()$fun
  infill.se = makeMBOInfillCritStandardError()$fun

  model = models[[iter]]
  type = vcapply(getOptPathDOB(opt.path), getType, iter = iter)
  idx.past = type %in% c("init", "seq")
  idx.pastpresent = type %in% c("init", "seq", "prop")

  # compute model prediction for current iter
  if (!inherits(model, "FailureModel")) {
    evals$yhat = ifelse(control$minimize, 1, -1) * infill.mean(evals.x, list(model), control)

    #FIXME: We might want to replace the following by a helper function so that we can reuse it in buildPointsData()
    if (propose.points == 1L) {
      evals[[infill.crit.id]] = opt.direction *
        critfun(evals.x, list(model), control, par.set, convertOptPathToDf(opt.path, control)[idx.past,, drop = FALSE])
    } else {
      objective = control$multipoint.moimbo.objective
      if (objective == "mean.dist") {
        evals[[infill.crit.id]] = opt.direction * infill.mean(evals.x, list(model), control, par.set, convertOptPathToDf(opt.path, control)[idx.past,, drop = FALSE])
      } else if (objective == "ei.dist") {
        evals[[infill.crit.id]] = opt.direction * infill.ei(evals.x, list(model), control, par.set, convertOptPathToDf(opt.path, control)[idx.past,, drop = FALSE])
      } else if (objective %in% c("mean.se", "mean.se.dist")) {
        evals[[infill.crit.id]] = opt.direction * infill.mean(evals.x, list(model), control, par.set, convertOptPathToDf(opt.path, control)[idx.past,, drop = FALSE])
      }
    }
    # prepare drawing of standard error (confidence interval)
    if (se) {
      evals$se = -infill.se(evals.x, list(model), control, par.set, convertOptPathToDf(opt.path, control)[idx.past,, drop = FALSE])
    }
  }

  if (isNumeric(par.set, include.int = FALSE)) {
    gg.fun = data.table::setDF(data.table::melt(evals, id.vars = c(getParamIds(opt.path$par.set), if (se) "se" else NULL)))

    if (control$multifid) {
      #rename .multifid.lvl according to control object
      repl = paste0(control$multifid.param, "=", control$multifid.lvls)
      names(repl) = as.character(seq_along(control$multifid.lvls))
      #gg.fun$.multifid.lvl = plyr::revalue(as.factor(gg.fun$.multifid.lvl), replace = repl)
      gg.fun$.multifid.lvl = factor(gg.fun$.multifid.lvl, labels = repl)
    }

    if (se) gg.fun$se = gg.fun$se * se.factor

    # if trafo for y is provided, indicate transformation on the y-axis
    ylab = name.y
    if (!is.null(trafo$y)) {
      ylab = paste0(name.y, " (", attr(trafo$y, "name"), "-transformed)")
    }
    #determine in wich pane (facet_grid) the points belong to
    pane.names = c(ylab, infill.crit.id)
    gg.fun$pane = factor(pane.names[ifelse(gg.fun$variable %in% c(name.y, "yhat"), 1, 2)], levels = pane.names)


    # data frame with points of different type (initial design points, infill points, proposed points)
    gg.points = buildPointsData(opt.path, iter)
    gg.points$pane = factor(pane.names[1], levels = pane.names)

    # transform y and yhat values according to trafo function
    if (!is.null(trafo$y)) {
      tr = trafo$y
      gg.fun[[name.y]] = tr(gg.fun[[name.y]])
      gg.points[[name.y]] = tr(gg.points[[name.y]])
    } else {
      tr = identity
    }

    # finally build the ggplot object(s)
    g = ggplot2::ggplot(data = gg.fun)
    next.aes = ggplot2::aes_string(x = names.x, y = "value", color = "as.factor(.multifid.lvl)", group = "paste(variable,.multifid.lvl)", linetype = "variable")
    if (!control$multifid) {
      next.aes = dropNamed(next.aes, c("group","colour"))
    }
    g = g + ggplot2::geom_line(next.aes, size = line.size)
    g = g + ggplot2::facet_grid(pane~., scales = "free")
    if (se && densregion) {
      #FIXME: We might lose transformation information here tr()
      next.aes = ggplot2::aes_string(x = names.x, ymin = "value-se", ymax = "value+se", group = ".multifid.lvl")
      if (!control$multifid) {
        next.aes = dropNamed(next.aes, "group")
      }
      g = g + ggplot2::geom_ribbon(data = gg.fun[gg.fun$variable == "yhat", ], next.aes, alpha = 0.2)
    }
    g = g + ggplot2::geom_point(data = gg.points, ggplot2::aes_string(x = names.x, y = name.y, colour = "type", shape = "type"), size = point.size)
    if (control$multifid) {
      mf.colors = tail(RColorBrewer::brewer.pal(n = length(control$multifid.lvls)+1, name = "PuBu"), -1)
      names(mf.colors) = levels(gg.fun$.multifid.lvl)
    } else {
      mf.colors = NULL
    }
    g = g + ggplot2::scale_colour_manual(values = c(mf.colors, colors), name = "type")
    g = g + ggplot2::scale_linetype(name = "type")

    if (noisy) {
      if (!anyMissing(x$y.true)) {
        source = data.frame(x$y.true)
        names(source) = name.y
        gap = calculateGap(source[idx.pastpresent, , drop = FALSE], global.opt, control)
      } else {
        gap = NA
      }
    } else {
      gap = calculateGap(convertOptPathToDf(opt.path, control)[idx.pastpresent,, drop = FALSE], global.opt, control)
    }

    g = g + ggplot2::ggtitle(sprintf("Iter = %i, Gap = %.4e", iter, gap))
    g = g + ggplot2::ylab(NULL)
    g = g + ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold")
    )

    plots = list(
      pl.fun = g
    )

  } else if (isDiscrete(par.set)) {
    if (!noisy) {
      stopf("Deterministic 1d function with a single factor parameter are not supported.")
    }

    gg.points = buildPointsData(opt.path, iter)

    if (se && densregion) {
      gg.points$se = -infill.se(gg.points[, names.x, drop = FALSE],
        models, control, par.set, opt.path[idx.past, , drop = FALSE])
      gg.points$se.min = gg.points[[name.y]] - se.factor * gg.points$se
      gg.points$se.max = gg.points[[name.y]] + se.factor * gg.points$se
    }

    pl.fun = ggplot2::ggplot(data = gg.points, ggplot2::aes_string(x = names.x, y = name.y, colour = "type", shape = "type"))
    pl.fun = pl.fun + ggplot2::geom_point(size = point.size)
    if (se && densregion) {
      pl.fun = pl.fun + ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "se.min", ymax = "se.max"), width = .1, alpha = .5)
    }

    pl.fun = pl.fun + ggplot2::xlab(names.x)
    pl.fun = pl.fun + ggplot2::scale_colour_discrete(name = "type")
    pl.fun = pl.fun + ggplot2::ggtitle(
      sprintf("Iter = %i, Gap = %.4e", iter,
      calculateGap(convertOptPathToDf(opt.path, control)[idx.pastpresent,, drop = FALSE], global.opt, control))
    )

    pl.fun = pl.fun + ggplot2::theme(
      legend.position = "top",
      legend.box = "horizontal",
      plot.title = ggplot2::element_text(size = 11, face = "bold")
    )

    plots = list(
      pl.fun = pl.fun
    )
  }
  return(plots)
}

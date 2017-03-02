#' @title Renders plots for exampleRun objects and displays them.
#'
#' @description
#'  Plots the intermediate states for 1-dimensional optimization problems
#'
#' @param object [\code{MBOExampleRun}]\cr
#'   \code{MBOExampleRun} object from \code{exampleRun}.
#' @param iters [\code{integer}]\cr
#'   Selected iterations of \code{object} to produce plots.
#'   Default is all iterations.
#' @param print [\code{logical(1)}]\cr
#'   Should the result be directly printed (\code{TRUE}) or returned as a list of ggplot objects (\code{FALSE})?
#'   Default is \code{TRUE}.
#' @param densregion [\code{logical(1)}]\cr
#'   Should the background be shaded? Default is \code{TRUE}.
#'   Only used if learner supports computation of standard error.
#' @param se.factor [\code{numeric(1)}]\cr
#'   If the model provides local standard error estimation,
#'   in addition to the mean response \code{yhat(x) +- se.factor * se(x)}
#'   is plotted above and below.
#'   Default is 1.
#' @param resolution [\code{integer(1)}]
#'   For how many points should the true objective function and the infill be calculated?
#'   The higher this number the more accurate the plot.
#'   For lower numbers the maxima of the infill might not be visible!
#' @param ... [any]\cr
#'   Currently not used.
#' @return Nothing.
#' @export
plotExampleRun = function(object, iters, print = TRUE, densregion = TRUE, se.factor = 1, resolution = 100) {

  iters.max = object$mbo.res$control$iters
  if (missing(iters)) {
    iters = seq_len(iters.max)
  } else {
    iters = asInteger(iters, lower = 0L, upper = iters.max, any.missing = FALSE)
  }
  assertFlag(print)
  assertFlag(densregion)
  assertNumber(se.factor, lower = 0)
  assertInt(resolution, lower = 1)

  run = object

  # start plot
  obj.fun = run$obj.fun
  obj.fun.mean = coalesce(smoof::getMeanFunction(obj.fun), obj.fun)
  par.set = getParamSet(run$obj.fun)
  control = run$mbo.res$control
  x.names = getParamIds(run$mbo.res$opt.path$par.set)
  x.types = getParamTypes(run$mbo.res$opt.path$par.set)
  x.name.numeric = x.names[x.types %in% ParamHelpers::getTypeStringsNumeric()]
  x.name.discrete = x.names[x.types %nin% ParamHelpers::getTypeStringsNumeric()]
  y.names = run$mbo.res$opt.path$y.names
  draw.design = generateGridDesign(par.set = par.set, resolution = resolution, trafo = FALSE)
  draw.design[[y.names[1]]] = vnapply(dfRowsToList(draw.design, par.set = par.set), obj.fun.mean)

  # opt path to data.frame
  op = run$mbo.res$opt.path
  op.df = as.data.frame(op)

  res = lapply(iters, function(iter) {
    draw.design.iter = draw.design
    # predict the mean of the outcome
    this.model = run$mbo.res$models[[iter]]
    this.design = op.df[this.model$subset, c(x.names, y.names)]
    prediction = predict(this.model, newdata = draw.design.iter)
    draw.design.iter$y.predict = getPredictionResponse(prediction)
    draw.design.iter$y.se = getPredictionSE(prediction)
    # get the infill crit value
    infill.crit = control$infill.crit$fun(points = draw.design.iter[, c(x.names, y.names)], models = list(this.model), control = control, par.set = par.set, design = this.design, iter = iter, attributes = TRUE)
    draw.design.iter$infill.value = setAttribute(infill.crit, "crit.components", NULL)
    draw.design.iter = cbind(draw.design.iter, attr(infill.crit, "crit.components"))

    # opt path for this iteration
    this.op.df = op.df[op.df$dob <= iter, ]
    # hacky subset OptPath
    this.op = deepCopyOptPath(op)
    this.op$env$path = this.op$env$path[seq_row(this.op.df), ]
    this.op$env$dob = this.op$env$dob[seq_row(this.op.df)]
    this.op$env$eol = this.op$env$eol[seq_row(this.op.df)]
    this.op.df$variable = y.names[1]
    this.op.df$state = ifelse(this.op.df$dob == 0, "init", ifelse(this.op.df$dob == iter, "prop", "seq"))
    # melt data for plotting
    mdata = reshape2::melt(draw.design.iter, measure.vars = c("y.predict", "infill.value"))
    # rename the melted columns according to input
    mdata$variable = plyr::revalue(mdata$variable, replace = c(y.predict = y.names[1], infill.value = control$infill.crit$id))
    mdata$variable = factor(mdata$variable, levels = c(y.names[1], control$infill.crit$id))
    this.op.df$variable = factor(this.op.df$variable, levels = c(y.names[1], control$infill.crit$id))
    # put name of discrete vector in front (like foo: a, foo: b, ...)
    if (length(x.name.discrete)>0) {
      mdata[[x.name.discrete]] = factor(mdata[[x.name.discrete]], labels = sprintf("%s: %s", x.name.discrete, levels(mdata[[x.name.discrete]])))
      this.op.df[[x.name.discrete]] = factor(this.op.df[[x.name.discrete]], labels = sprintf("%s: %s", x.name.discrete, levels(this.op.df[[x.name.discrete]])))
    }

    # calculate gap if possible
    opt.value = getGlobalOptimum(obj.fun)$value
    if (!is.null(opt.value) && !is.na(opt.value)) {
      gap = opt.value - getOptPathY(op)[chooseFinalPoint2(control = control, opt.path = this.op, model = this.model, task = makeTaskSingleObj(opt.path = this.op, control = control))]
      gap.subtitle = sprintf("Gap: %.4g", gap)
    } else {
      gap.subtitle = character()
    }

    g = ggplot()
    if (densregion) {
      # uncertainty area (plot first so everything else is above)
      g = g + geom_ribbon(data = mdata[mdata$variable == y.names[1],], aes_string(ymin = "value - se * se.factor", ymax = "value + se * se.factor", x = x.name.numeric), fill = "black", alpha = 0.2)
    }
    # lines for the mean prediction- and the infill criteria values
    g = g + geom_line(data = mdata, mapping = aes_string(x = x.name.numeric, y = "value"))
    # lines for the true objective function values
    g = g + geom_line(data = mdata[mdata$variable == y.names[1],], mapping = aes_string(x = x.name.numeric, y = y.names[1]), linetype = 2)
    # points for past evaluations
    g = g + geom_point(data = this.op.df, mapping = aes_string(x = x.name.numeric, y = y.names, shape = "state", color = "state"), size = 3)
    # split categorical to horizontal and function/infill vertically
    g = g + facet_grid(as.formula(sprintf("%s~%s", "variable", if (length(x.name.discrete)>0) x.name.discrete else ".")), scales = "free")
    # drop "y" from y axis as it's in the grid label
    g = g + theme(axis.title.y = element_blank())
    # Title Information
    g = g + labs(title = sprintf("%s, Iteration: %s", getName(run$obj.fun), iter), subtitle = gap.subtitle)
    g
  })

  if (print) {
    for (g in res) {
      print(g)
    }
  } else {
    return(res)
  }

}

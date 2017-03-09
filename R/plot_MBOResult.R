#' @title MBO Result Plotting
#'
#' @description
#' Plots any MBO result objects. Plots for X-Space, Y-Space and any coloumn in
#' the optimization path are available. This function uses
#' \code{\link[ParamHelpers]{plotOptPath}} from package \code{ParamHelpers}.
#'
#' @name plotMBOResult
#' @rdname plotMBOResult
#'
#'
#' @param x [\code{MBOResult}]\cr
#'   \code{MBOSingleObjResult} or \code{MBOMultiObjResult} object.
#' @param iters [\code{integer}]\cr
#'   Iterations to be plotted, 0 indicates the initial design. Default is all iterations.
#' @param pause [\code{logical(1)}]\cr
#'   Should the process be paused after each iteration?
#'   Default is \code{interactive()}.
#' @param ...
#'  Additional parameters for the \code{\link[ParamHelpers]{plotOptPath}}
#'  function in package \code{ParamHelpers}.
#'
NULL

#' @rdname plotMBOResult
#' @export
plot.MBOSingleObjResult = function(x, iters = NULL, pause = interactive(), ...) {

  # extract and set params
  opt.path = x$opt.path
  control = x$control
  y.names = opt.path$y.names
  infill.crit.id = getMBOInfillCritId(control$infill.crit)

  if (is.null(iters))
    iters = max(getOptPathDOB(opt.path))

  args = list(...)
  args = insert(args, list(op = opt.path, iters = iters, pause = pause))
  args = insert(list(y.over.time = list(y.names, infill.crit.id), title = "MBO Result"), args)

  do.call(plotOptPath, args)
}

#' @rdname plotMBOResult
#' @export
plot.MBOMultiObjResult = function(x, iters = NULL, pause = interactive(), ...) {

  # extract and set params
  opt.path = x$opt.path
  control = x$control
  y.names = opt.path$y.names
  infill.crit.id = getMBOInfillCritId(control$infill.crit)

  if (control$multiobj.method == "mspot") {
    infill.crit = paste(infill.crit.id, opt.path$y.names, sep = ".")
  } else {
    infill.crit = infill.crit.id
  }

  if (is.null(iters))
    iters = max(getOptPathDOB(opt.path))

  args = list(...)
  args = insert(args, list(op = opt.path, iters = iters, pause = pause))
  args = insert(list(y.over.time = list(y.names, infill.crit), title = "MBO Result"), args)

  do.call(plotOptPath, args)
}

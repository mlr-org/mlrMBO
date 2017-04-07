# single-objective
#' @export
renderExampleRunPlot.MBOExampleRun = function(object, iter, densregion = TRUE,
  se.factor = 1, single.prop.point.plots = FALSE, xlim = NULL, ylim = NULL,
  point.size = 3, line.size = 1, trafo = NULL, colors = c("red", "blue", "green"), ...) {

  requirePackages("ggplot2")

  iters.max = object$control$iters
  iter = asInt(iter, lower = 1L, upper = iters.max)
  assertFlag(densregion)
  assertNumber(se.factor, lower = 0)
  assertNumber(point.size, lower = 1)
  assertNumber(line.size, lower = 1)
  assertCharacter(colors, len = 3L, any.missing = FALSE)

  if (!is.null(xlim)) {
    assertNumeric(xlim, len = 2L, any.missing = FALSE)
  }
  if (!is.null(ylim)) {
    assertNumeric(ylim, len = 2L, any.missing = FALSE)
  }

  n.params = object$n.params
  par.types = object$par.types
  par.set = object$par.set
  trafo = buildTrafoList(n.params, trafo)

  if (n.params == 1L) {
    if (par.types %nin% c("numeric", "numericvector", "discrete", "discretevector")) {
      stopf("For 1D function only plotting of numeric or discrete functions possible, but your function is '%s'.", par.types)
    }
    return(renderExampleRunPlot1d(object, iter = iter, xlim = xlim, ylim = ylim, se.factor = se.factor, pause = pause,
      point.size = point.size, line.size = line.size, trafo = trafo, densregion = densregion,
      colors = colors, ...))
  } else if (n.params == 2L) {
    if (!hasNumeric(par.set)) {
      stopf("At least one parameter of the target function must be numeric!")
    }
    return(renderExampleRunPlot2d(object, iter = iter, xlim = xlim, ylim = ylim, se.factor = se.factor, pause = pause,
      point.size = point.size, line.size = line.size, trafo = trafo,
      colors = colors, ...))
  } else {
    stopf("Functions with greater than 3 parameters are not supported.")
  }
}

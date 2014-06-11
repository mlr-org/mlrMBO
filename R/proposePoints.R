# Propose infill points.
#
# @param model [\code{\link{WrappedModel}}]\cr
#   Model used for prediction.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Collection of parameters and their constraints for optimization.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object for mbo.
# @param opt.path [\code{\link[ParamHelpers]{OptPath}}]\cr
#   Optimization path to save of type \code{\link[ParamHelpers]{OptPath}}.
# @return [\code{data.frame}]
#   New infill points.
proposePoints = function(model, par.set, control, opt.path, ...) {
  # generate a few random points if model failed
  if (inherits(model, "FailureModel"))
    return(generateDesign(control$propose.points, par.set, randomLHS, ints.as.num = TRUE))

  design = as.data.frame(opt.path)
  design$dob = design$eol = design$error.message = NULL
  design = convertDataFrameCols(design, ints.as.num = TRUE, logicals.as.factor = TRUE)

  if (control$propose.points == 1L) {
    # determine infill criterion
    infill.crit.fun = getInfillCritFunction(control$infill.crit)

    # determine infill optimization strategy
    infill.opt.fun = getInfillOptFunction(control$infill.opt)

    prop.points = infill.opt.fun(infill.crit.fun, model, control, par.set, opt.path, design, ...)
    prop.points.crit.values = infill.crit.fun(prop.points, model, control, par.set, design, ...)
  } else {
    multipoint.infill.opt.fun = getMultipointInfillOptFunction(control$multipoint.method)
    prop.design = multipoint.infill.opt.fun(model, control, par.set, opt.path, design, ...)
    prop.points = prop.design$prop.points
    prop.points.crit.values = prop.design$prop.points.crit.values
  }
  return(list(prop.points = prop.points, prop.points.crit.values = prop.points.crit.values))
}

# Optimizers for multipoint infill criteria

# General interface
#
# @param model [\code{\link{WrappedModel}}]\cr
#   Model fitted on design.
# @param control [\code{\link{MBOControl}}]\cr
#   Control object.
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Parameter set.
# @param opt.path [\code{\link[ParamHelpers{OptPath}}]\cr
#   Optimization path / archive.
# @param design [\code{data.frame}]\cr
#   Design of already visited points.
# @return [\code{data.frame}]. Proposed points that should be evaluated.

# Use LCB single crit but sample multiple different lambdas
# NOTE THAT WE MIGHT PRODUCE LESS POINS THERN REQUESTED IF SOME OF THEM ARE EXTREMELY CLOSE
# FIXME: document this. also maybe improve?
multipointInfillOptLCB = function(model, control, par.set, opt.path, design, ...) {
  # copy control and optimize multiple times with singlecrit lcb / different lambda
  control2 = control
  control2$propose.points = 1L
  control2$infill.crit = "lcb"
  newdes = data.frame()
  lambdas = c()
  crit.vals = c()
  iter = 1L

  #FIXME: could be done in parallel
  while (iter <= control$propose.points) {
    # draw lambda from exp dist
    control2$infill.crit.lcb.lambda = rexp(1)
    newdes1 = proposePoints(model, par.set, control2, opt.path)
    prop.points = newdes1$prop.points

    # as we might construct the same xs for similar lamba, we
    # require that a new point is not nearly the same as another proposed one
    if (nrow(newdes) > 0L) {
      # FIXME: what do we here for factor vars, wrt dist?
      dists = apply(newdes, 1, function(x) min(abs(x - prop.points[1, ])))
    } else {
      dists = Inf
    }

    # if LCB produced the "same" newdes-x-value twice, bad luck for it.
    # in that case we return less points to eval

    #FIXME: this min value is currently not exported in control
    if (min(dists) > control$lcb.min.dist) {
      newdes = rbind(newdes, prop.points)
      lambdas = c(lambdas, control2$infill.crit.lcb.lambda)
      crit.vals = c(crit.vals, newdes1$crit.vals)
    }
    iter = iter + 1L
  }
  newdes = setAttribute(newdes, "multipoint.lcb.lambdas", lambdas)
  list(prop.points = newdes, crit.vals = crit.vals)
}

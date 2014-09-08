# FIXME: For the moment excluded - talk about it
# Random Search for the multicrit optimization of all models
#infillOptMultiCritRandom = function(infill.crit, models, control, par.set, opt.path, design, ...) {
#  requirePackages("emoa")
#
#  newdesign = generateDesign(control$infill.opt.multicrit.randomsearch.points, par.set,
#    randomLHS)
#
#  FUN.VALUE = rep(0, control$infill.opt.multicrit.randomsearch.points)
#  ys = vapply(models, infill.crit, FUN.VALUE = FUN.VALUE, points = newdesign, control = control,
#    par.set = par.set, design = design, ...)
#
#  front.inds = !is_dominated(t(ys))
#  return(list(
#    points = recodeTypes(newdesign[front.inds, , drop = FALSE], par.set),
#    crit.vals = ys[front.inds, , drop = FALSE])
#  )
#}


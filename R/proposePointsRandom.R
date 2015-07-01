proposePointsRandom = function(models, par.set, control, opt.path, iter) {
  if (control$infill.crit == "random")
    n = control$propose.points
  else
    n = control$interleave.random.points
  list(
    prop.points = generateRandomDesign(par.set = par.set, n = n),
    crit.vals = matrix(rep.int(NA_real_, n), nrow = n, ncol = 1L),
    propose.time = rep.int(NA_real_, n),
    errors.model = rep.int(NA_character_, n)
  )
}

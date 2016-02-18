# Proposes n points randomly by calling generateRandomDesign.
# crit.vals, propose.time and errors.model are all set to NA
proposePointsRandom = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptProblemParSet(opt.problem)
  n = control$interleave.random.points
  list(
    prop.points = generateRandomDesign(par.set = par.set, n = n),
    crit.vals = matrix(rep.int(NA_real_, n), nrow = n, ncol = 1L),
    propose.time = rep.int(NA_real_, n),
    prop.type = rep("random_interleave", n),
    errors.model = rep.int(NA_character_, n)
  )
}

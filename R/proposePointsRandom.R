# This function emulates the behaviour of all proposePoints* functions
# The return value is a list with specific NAs so that the opt.path can be written
proposePointsRandom = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptProblemParSet(opt.problem)  
  n = control$interleave.random.points
  list(
    prop.points = generateRandomDesign(par.set = par.set, n = n),
    crit.vals = matrix(rep.int(NA_real_, n), nrow = n, ncol = 1L),
    propose.time = rep.int(NA_real_, n),
    errors.model = rep.int(NA_character_, n)
  )
}

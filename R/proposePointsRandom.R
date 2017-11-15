# Proposes n points randomly by calling generateRandomDesign.
# crit.vals, propose.time and errors.model are all set to NA
proposePointsRandom = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptStateParSet(opt.state)
  n = control$interleave.random.points
  makeProposal(
    control = control,
    prop.points = generateRandomDesign(par.set = par.set, n = n),
    prop.type = rep("random_interleave", n)
  )
}

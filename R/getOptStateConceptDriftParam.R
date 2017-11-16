getOptStateConceptDriftParam = function(opt.state) {
  dob = getOptStateLoop(opt.state)
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  if (!is.null(getOptProblemDriftParam(opt.problem))) {
    res = list(control$conceptdrift.drift.function(dob))
    res = setNames(res, getOptProblemDriftParam(opt.problem))
  } else {
    res = list()
  }
  res
}

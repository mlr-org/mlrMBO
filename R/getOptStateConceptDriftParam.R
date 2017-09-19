getOptStateConceptDriftParam = function(opt.state) {
  dob = getOptStateLoop(opt.state)
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  if (!is.null(control$conceptdrift.drift.param)) {
    res = list(control$conceptdrift.drift.function(dob))
    res = setNames(res, control$conceptdrift.drift.param)
  } else {
    res = list()
  }
  res
}

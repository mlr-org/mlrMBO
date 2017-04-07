#FIXME: Doku

makeTasks = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  if (control$n.objectives == 1L) {
    tasks = list(makeTaskSingleObj(opt.path, control))
  } else {
    if (control$multiobj.method == "parego")
      tasks = makeTasksParEGO(opt.path, control, all.possible.weights = getOptProblemAllPossibleWeights(opt.problem))
    else
      tasks = makeTasksMultiObj(opt.path, control)
  }
  return(tasks)
}

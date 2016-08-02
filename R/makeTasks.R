#FIXME: Doku

makeTasks = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  if (control$n.objectives == 1L) {
    tasks = list(makeTaskSingleObj(opt.path, control))
  } else {
    if (control$multicrit.method == "parego") {
      tasks = makeTasksParEGO(opt.path, control, all.possible.weights = getOptProblemAllPossibleWeights(opt.problem))
      
    } else if (control$multicrit.method %in% c("epic", "epo")) {
      tasks = list(makeTasksMultiCritClassif(opt.path, control))
    } else {
      tasks = makeTasksMultiCrit(opt.path, control)
    }
  }
  return(tasks)
}

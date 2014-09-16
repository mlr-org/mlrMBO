makeTasks = function(par.set, opt.path, algo.init, control) {
  if (control$number.of.targets == 1L) {
    tasks = list(makeTaskSingleObj(par.set, opt.path, control))
  } else {
    if (control$multicrit.method == "parego")
      tasks = makeTasksParEGO(par.set, opt.path, control, algo.init$all.possible.weights)
    else
      tasks = makeTasksMultiCrit(par.set, opt.path, control)
  }
  return(tasks)
}

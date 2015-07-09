#FIXME: Doku

makeTasks = function(opt.path, algo.init, control) {
  if (control$number.of.targets == 1L) {
    tasks = list(makeTaskSingleObj(opt.path, control))
  } else {
    if (control$multicrit.method == "parego")
      tasks = makeTasksParEGO(opt.path, control, algo.init$all.possible.weights)
    else
      tasks = makeTasksMultiCrit(opt.path, control)
  }
  return(tasks)
}

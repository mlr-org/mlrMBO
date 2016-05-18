mboAsynTemplate = function(obj) {
  UseMethod("mboAsynTemplate")
}

mboAsynTemplate.OptProblem = function(obj) {
  opt.problem = obj
  #sanity checks here
  control = getOptProblemControl(opt.problem)
  if (control$iters == Inf && control$schedule.nodes == 1) {
    stopf("iters are inf and schedule.nodes is 1. Please set on of those.")
  }
  
  #start the calculation
  writeThingToDirectory(opt.problem, opt.problem, "opt_problem", hash = FALSE)
  opt.state = makeOptState(opt.problem = opt.problem)
  #parallized in evalTargetFun with level mlrMBO.feval
  evalMBODesign.OptState(opt.state)
  writeThingToDirectory(opt.problem, getOptStateOptPath(opt.state), "state_0_init", hash = FALSE)
  mboAsynTemplate(opt.state)
}

mboAsynTemplate.OptState = function(obj) {
  opt.state = obj
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  #check if init files are already there, otherwise create them
  if (!file.exists(file.path(getAsynDir(opt.problem), "opt_problem.rds"))) {
    writeThingToDirectory(opt.problem, opt.problem, "opt_problem", hash = FALSE)
  }
  if (!file.exists(file.path(getAsynDir(opt.problem), "state_0_init.rds"))) {
    writeThingToDirectory(opt.problem, getOptStateOptPath(opt.state), "state_0_init", hash = FALSE)
  }

  #wrapper to check budget
  asynSingle = function(i) {
    opt.state = readDirectoryToOptState(opt.problem)
    start.after = ifelse(i < control$schedule.nodes, i - 1, 0)
    ##! control$schedule.nodes
    if (!shouldTerminate.OptState(opt.state)$term) {
      runMBOOnline(opt.state, start.after = start.after)
    }
    invisible()
  }

  #infinityLoop wrapper
  asynInfinityLoop = function(i) {
    start.after = ifelse(i < control$schedule.nodes, i - 1, 0)
    repeat {
      opt.state = readDirectoryToOptState(opt.problem)
      if (shouldTerminate.OptState(opt.state)$term) break
      runMBOOnline(opt.state, start.after = start.after, node = i)
    }
    invisible()
  }
  
  if (!is.null(control$batchJobs.reg)) {
    #write method to send further jobs if budget is not exhausted and number of waiting jobs gets low
  } else {
    parallelMap(asynInfinityLoop, i = seq_len(control$schedule.nodes), level = "mlrMBO.asyn")
    opt.state = readDirectoryToOptState(opt.problem)
  }
  cleanDirectory(opt.problem)
  return(opt.state)
}

runMBOOnline = function(x, ...) {
  UseMethod("runMBOOnline")
}

runMBOOnline.character = function(x, ...) {
  runMBOOnline(readRDS(file.path(x, "opt_problem.rds")), ...)
}

runMBOOnline.OptProblem = function(x, start.after = 0, ...) {
  runMBOOnline(readDirectoryToOptState(x, read.at.least = start.after), ...)
}

runMBOOnline.OptState = function(x, node = NA_integer_, ...) {
  opt.state = x
  prop = proposePoints.OptState(opt.state)
  prop$extra$scheduled.on = node
  prop$extra$eval.state = "proposed"
  if (is.na(node)) {
    prop.prefix = "prop_"
  } else {
    prop.prefix = sprintf("node_%i_prop_", node)
  }
  proposal.file = writeThingToDirectory(getOptStateOptProblem(opt.state), prop, prop.prefix)
  prop$extra$eval.state = "done"
  evalProposedPoints.OptState(opt.state, prop)
  finalizeMboLoop(opt.state)
  unlink(proposal.file)
  writeResultToDirectory(opt.state)
  invisible(opt.state)
}
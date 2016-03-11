mboAsynTemplate = function(opt.problem) {
  #sanity checks here
  control = getOptProblemControl(opt.problem)
  #if (!(control$infill.crit == "cb" && control$multipoint.method == "cb")) {
  #  stopf("We don't have CL support or similar now, so only cb with multipoint.method == cb makes sense!")
  #}
  if (control$time.budget < Inf && control$exec.time.budget == Inf) {
    stopf("We don't support time.budget yet. Please use 'exec.time.budget' instead")
  }
  if (control$iters == Inf && control$schedule.nodes == 1) {
    stopf("iters are inf and schedule.nodes is 1. Please set on of those.")
  }
  
  #wrapper to check budget
  wrapAsynFun = function() {
    opt.state = readDirectoryToOptState(opt.problem)
    if (!shouldTerminate.OptState(opt.state)$term) {
      runMBOOnline(opt.state)
    }
  }
  
  #start the calculation
  setupMBOOnline(opt.problem)

  if (control$iters < Inf) {
    parallelMap(function(x) wrapAsynFun(), x = rep(TRUE, control$iters), level = "mlrMBO.asyn")
    opt.state = readDirectoryToOptState(opt.problem)
  } else if (!is.null(control$batchJobs.reg)) {
    #write method to send further jobs if budget is not exhausted and number of waiting jobs gets low
  } else {
    opt.state = readDirectoryToOptState(opt.problem)
    while(!shouldTerminate.OptState(opt.state)$term){
      parallelMap(function(x) wrapAsynFun(), x = rep(TRUE, control$schedule.nodes * 10), level = "mlrMBO.async")
      opt.state = readDirectoryToOptState(opt.problem)
    }
  }
  cleanDirectory(opt.problem)
  return(opt.state)
}

setupMBOOnline = function(opt.problem) {
  path = dirname(getOptProblemControl(opt.problem)$save.file.path)
  dir.create(path, showWarnings = FALSE)
  writeThingToDirectory(opt.problem, opt.problem, "opt_problem", hash = FALSE)
  opt.state = makeOptState(opt.problem = opt.problem)
  #parallized in evalTargetFun with level mlrMBO.feval
  evalMBODesign.OptState(opt.state)
  writeThingToDirectory(opt.problem, getOptStateOptPath(opt.state), "state_0_init", hash = FALSE)
}


runMBOOnline = function(x, ...) {
  UseMethod("runMBOOnline")
}

runMBOOnline.character = function(x, ...) {
  runMBOOnline(readRDS(file.path(x, "opt_problem.rds")))
}

runMBOOnline.OptProblem = function(x, ...) {
  runMBOOnline(readDirectoryToOptState(x))
}

runMBOOnline.OptState = function(x, ...) {
  opt.state = x
  setOptStateLoop(opt.state) #loop + 1
  prop = proposePoints.OptState(opt.state)
  proposal.file = writeThingToDirectory(getOptStateOptProblem(opt.state), prop, "prop_")
  evalProposedPoints.OptState(opt.state, prop)
  unlink(proposal.file)
  writeResultToDirectory(opt.state)
  invisible(opt.state)
}
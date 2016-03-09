mboAsynTemplate = function(opt.problem) {
  #sanity checks here
  control = getOptProblemControl(opt.problem)
  if (!(control$infill.crit == "cb" && control$multipoint.method == "cb")) {
    stopf("We don't have CL support or similar now, so only cb with multipoint.method == cb makes sense!")
  }
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
    parallelMap(function(x) wrapAsynFun(), x = rep(TRUE, control$iters), level = "mlrMBO.async")
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
  return(opt.state)
}

setupMBOOnline = function(opt.problem) {
  path = dirname(getOptProblemControl(opt.problem)$save.file.path)
  dir.create(path, showWarnings = FALSE)
  saveRDS(opt.problem, file = file.path(path, sprintf("opt_problem.rds")))
  opt.state = makeOptState(opt.problem = opt.problem)
  #parallized in evalTargetFun with level mlrMBO.feval
  evalMBODesign.OptState(opt.state)
  saveRDS(getOptStateOptPath(opt.state), file = file.path(path, sprintf("state_0_init.rds")))
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
  evalProposedPoints.OptState(opt.state, prop)
  writeResultToDirectory(opt.state)
  invisible(opt.state)
}

readDirectoryToOptState = function(opt.problem) {
  control = getOptProblemControl(opt.problem)
  readOptPathFromDirectory = function(path) {
    op = readRDS(file.path(path, "state_0_init.rds"))
    files = list.files(path, pattern = "^state_[[:alnum:]]+\\.rds$", full.names = TRUE)
    file.contents = lapply(files, readRDS)
    for (op.el in file.contents) {
      do.call(addOptPathEl, c(list(op = op), op.el))
    }
    return(op)
  }
  path = dirname(control$save.file.path)
  opt.path = readOptPathFromDirectory(path)
  makeOptState(
    opt.problem = opt.problem, 
    opt.path = opt.path, 
    loop = getOptPathLength(opt.path)
    )
}

writeResultToDirectory = function(opt.state) {
  opt.path = getOptStateOptPath(opt.state)
  last.op.el = getOptPathEl(opt.path, getOptPathLength(opt.path))
  hash = substr(digest::sha1(list(Sys.time(), Sys.info())), 1, 50) #filenames will have length 64
  path = dirname(getOptProblemControl(getOptStateOptProblem(opt.state))$save.file.path)
  saveRDS(last.op.el, file = file.path(path, sprintf("state_%.4i%s.rds", getOptStateLoop(opt.state), hash)))
}

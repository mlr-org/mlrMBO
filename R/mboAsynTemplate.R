mboAsynTemplate = function(opt.problem) {
  #sanity checks here
  control = getOptProblemControl(opt.problem)
  if (!(control$infill.crit == "cb" && control$multipoint.method == "cb")) {
    stopf("We don't have CL support or similar now, so only cb with multipoint.method == cb makes sense!")
  }
  setupMBOOnline(opt.problem)
  opt.state = readDirectoryToOptState(opt.problem)
  
  while(!shouldTerminate.OptState(opt.state)$term){
    #not really:
    runMBOOnline(opt.problem)
    opt.state = readDirectoryToOptState(opt.problem)
  }

}

setupMBOOnline = function(opt.problem) {
  path = dirname(getOptProblemControl(opt.problem)$save.file.path)
  dir.create(path, showWarnings = FALSE)
  saveRDS(opt.problem, file = file.path(path, sprintf("opt_problem.rds")))
  opt.state = makeOptState(opt.problem = opt.problem)
  #parallized in evalTargetFun with level mlrMBO.feval
  evalMBODesign.OptState(opt.state)
  saveRDS(opt.problem, file = file.path(path, sprintf("state_0.rds")))
}


runMBOOnline = function(x, ...) {
  UseMethod("runMBOOnline")
}

runMBOOnline.character = function(x, ...) {
  runMBOOnline(readRDS(file.path(x, "opt_problem.rds")))
}

runMBOOnline.OptProblem = function(x, ...) {
  opt.state = readDirectoryToOptState(x)
  setOptStateLoop() #loop + 1
  prop = proposePoints.OptState(opt.state)
  evalProposedPoints.OptState(opt.state, prop)
  writeResultToDirectory(opt.state)
  invisible(opt.state)
}

readDirectoryToOptState = function(opt.problem) {
  readOptPathFromDirectory = function(path) {
    files = list.files(path, pattern = "^state_[[:alnum:]]+\\.rds$", full.names = TRUE)
    op = makeMBOOptPath(x)
    if (length(files)) {
      df = rbindlist(lapply(files, readRDS), fill = TRUE)
      op$env$path = as.data.frame(df[, c(x$control$y.name, getParamIds(getOptProblemParSet(x))), with = FALSE])
      op$env$dob = df$dob
      op$env$eol = df$eol
      op$env$error.message = df$error.message
      op$env$exec.time = df$exec.time
      op$env$extra = vector("list", nrow(df))
    }
    return(op)
  }
  path = dirname(getOptProblemControl(opt.problem)$save.file.path)
  opt.path = readOptPathFromDirectory(path)
  makeOptState(
    opt.problem = x, 
    opt.path = opt.path, 
    loop = getOptPathLength(getOptStateOptPath(opt.state))
    )
}

writeResultToDirectory = function(opt.state) {
  last.row = getOptPathEl(opt.path, getOptPathLength(getOptStateOptPath(opt.state)))
  hash = digest::sha1(list(Sys.time(), Sys.info()))
  last.row$extra = NULL
  saveRDS(as.data.frame(last.row), file = file.path(path, sprintf("state_%i_%s.rds", getOptStateLoop(opt.state), hash)))
}
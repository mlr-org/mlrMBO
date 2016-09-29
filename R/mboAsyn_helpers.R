readOptPathFromDirectory = function(path) {
  op = readRDS(file.path(path, "state_0_init.rds"))
  files = list.files(path, pattern = "^state_[[:alnum:]]+\\.rds$", full.names = TRUE)
  file.contents = lapply(files, readRDS)
  for (op.el in file.contents) {
    do.call(addOptPathEl, c(list(op = op), op.el))
  }
  return(op)
}

listProposals = function(path) {
  list.files(path, pattern = "(^|_)prop_[[:alnum:]]+\\.rds$", full.names = TRUE)
}

readFileContents = function(files) {
  file.contents = lapply(files, function(f) {
    tryCatch({readRDS(f)}, error = function(e) return(NULL))
  })
  file.contents[!sapply(file.contents, is.null)]
}

readProposalsFromDirectoryToOptPath = function(opt.path, opt.problem) {
  control = getOptProblemControl(opt.problem)
  path = getAsynDir(opt.problem)
  files = listProposals(path)
  file.contents = readFileContents(files)

  # for friggn imputation build a data.frame we can impute on
  df = convertOptPathToDf(opt.path)
  n = nrow(df)
  df.props = do.call(rbind, extractSubList(file.contents, "prop.points", simplify = FALSE))
  df = plyr::rbind.fill(df, df.props) #FIXME: I am not totally sure if the x will have the right types in complicated cases
  # user requested handling of NA y values in Opt-Path
  if (anyNA(df[[control$y.name]])) {
    impute.y = switch(control$asyn.impute.method,
      noisymean = asynImputeNoisyMean,
      quantilemean = asynNoImpute,
      mc = asynNoImpute,
      mean = asynImputeMean,
      min = asynImputeMin,
      max = asynImputeMax)
    df[[control$y.name]] = impute.y(opt.problem, data = df)
  }
  last.extra = getOptPathEl(opt.path, getOptPathLength(opt.path))$extra #FIXME: We just cheat and copy last 
  last.extra$prop.type = "evaluating"
  par.set = getOptProblemParSet(opt.problem)
  for (i in seq_along(file.contents)) {
    x = dfRowToList(file.contents[[i]]$prop.points, par.set, 1)
    y = df[[control$y.name]][n + i]
    addOptPathEl(opt.path, x = x, y = y, extra = last.extra, exec.time = 0) 
  }
}

hashOptPath = function(opt.path, ignore.proposed = TRUE) {
  prop.type = getOptPathCol(opt.path, "prop.type")
  y = getOptPathY(opt.path)
  if (ignore.proposed) {
    y = y[prop.type == "done"] #we ignore proposed y as they may varey
  }
  digest::sha1(list(
    getOptPathX(opt.path),
    y,
    getOptPathDOB(opt.path),
    prop.type 
  ))
}

readDirectoryToOptState = function(opt.problem, time.out = 60) {
  #FIXME: Blocking leads to error in time.budget?
  start.time = as.numeric(Sys.time(), units = "secs")
  control = getOptProblemControl(opt.problem)
  repeat {
    opt.path = readOptPathFromDirectory(getAsynDir(opt.problem))
    max.dob = max(getOptPathDOB(opt.path))
    #add proposals with CL or NAs for unevaluated y to opt.path
    readProposalsFromDirectoryToOptPath(opt.path, opt.problem)
    #if we do not have to look fore identical opt.paths: break and continue normal
    if (!control$asyn.wait.for.proposals) break
    block.files = list.files(getAsynDir(opt.problem), pattern = "(^|_)block_[[:alnum:]]+\\.rds$", full.names = TRUE)
    #if there aint any opt.states blocked: break and continue normal
    if (length(block.files)==0) break
    #if there aint any similiar opt.state: break and continue normal
    file.contents = readFileContents(block.files)
    if (length(file.contents)==0) break
    op.hashes = lapply(file.contents, function(f) hashOptPath(getOptStateOptPath(f)))
    this.op.hash = hashOptPath(opt.path)
    if (this.op.hash %nin% op.hashes) break
    #we don't want to read an similar opt.state. That's why we wait until proposals or results have been generated to get another opt.state.
    # unless we have a time out
    if (as.numeric(Sys.time(), units = "secs") - start.time > time.out) break
    Sys.sleep(1)
  }
  #find the first available exec.timestamp which does not belong to init.design
  if (max.dob > 0) {
    exec.timestamps = getOptPathCol(opt.path, "exec.timestamp", dob = seq_len(max.dob))
    start.time = min(c(start.time, exec.timestamps), na.rm = TRUE)
  }
  makeOptState(
    opt.problem = opt.problem, 
    opt.path = opt.path, 
    loop = max.dob + 1,
    time.used = as.numeric(Sys.time(), units = "secs") - start.time
  )
}

writeResultToDirectory = function(opt.state) {
  opt.path = getOptStateOptPath(opt.state)
  last.op.el = getOptPathEl(opt.path, getOptPathLength(opt.path))
  prefix = sprintf("state_%.4i", getOptStateLoop(opt.state))
  writeThingToDirectory(getOptStateOptProblem(opt.state), last.op.el, prefix)
}

writeThingToDirectory = function(opt.problem, thing, prefix, hash = TRUE){
  if (hash) {
    hash = substr(digest::sha1(list(Sys.time(), Sys.info(), Sys.getpid())), 1, 64-nchar(prefix)-4) #filenames will have length 64
  } else {
    hash = ""
  }
  path = getAsynDir(opt.problem)
  save.file = file.path(path, sprintf("%s%s.rds", prefix, hash))
  saveRDS(thing, file = save.file)
  save.file
}

cleanDirectory = function(opt.problem) {
  ctrl = getOptProblemControl(opt.problem)
  if (is.null(ctrl$asyn.cleanup) || ctrl$asyn.cleanup == TRUE) {
    path = getAsynDir(opt.problem)
    unlink(path, recursive = TRUE, force = TRUE)
  }
}

cleanProposals = function(opt.problem) {
  path = getAsynDir(opt.problem)
  files = listProposals(path)
  unlink(files, recursive = FALSE, force = FALSE)
}

getAsynDir = function(opt.problem) {
  path = file.path(dirname(getOptProblemControl(opt.problem)$save.file.path), "asyn")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  path
}

asynImputeMean = function(opt.problem, data) {
  y.name = getOptProblemControl(opt.problem)$y.name
  learner = getOptProblemLearner(opt.problem)
  cols = setNames(list(imputeLearner(learner, features = NULL)), y.name)
  impute(data, cols = cols)$data[[y.name]]
}

asynImputeMin = function(opt.problem, data) {
  y.name = getOptProblemControl(opt.problem)$y.name
  impute(data, cols = setNames(list(imputeMin(0)), y.name))$data[[y.name]]
}

asynImputeMax = function(opt.problem, data) {
  y.name = getOptProblemControl(opt.problem)$y.name
  impute(data, cols = setNames(list(imputeMax(0)), y.name))$data[[y.name]]
}

asynImputeNoisyMean = function(opt.problem, data) {
  y.name = getOptProblemControl(opt.problem)$y.name
  learner = getOptProblemLearner(opt.problem)
  learner = setPredictType(learner, predict.type = "se")
  this.features = NULL
  imputeNoisyLearner = imputeLearner(learner, features = this.features)
  imputeNoisyLearner$impute = function(data, target, col, model, features) {
    x = data[[col]]
    ind = is.na(x)
    # if no NAs are present in data, we always return it unchanged
    if (all(!ind)) return(x)
    # FIXME: we do get a list instead of a data.frame?
    newdata = as.data.frame(data)[ind, features, drop = FALSE]
    p = predict(model, newdata = newdata)
    p = rnorm(nrow(newdata), mean = getPredictionResponse(p), sd = getPredictionSE(p))
    replace(x, ind, p)
  }
  cols = setNames(list(imputeNoisyLearner), y.name)
  impute(data, cols = cols)$data[[y.name]]
}

asynNoImpute = function(opt.problem, data) {
  y.name = getOptProblemControl(opt.problem)$y.name
  data[[y.name]]
}

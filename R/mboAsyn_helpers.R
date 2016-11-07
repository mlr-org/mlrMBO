readOptPathFromDirectory = function(path) {
  op = readRDS(file.path(path, "state_0_init.rds"))
  files = list.files(path, pattern = "^state_[0-9]+_[0-9]+_[[:alnum:]]+\\.rds$", full.names = TRUE)
  file.contents = lapply(files, readRDS)
  for (op.el in file.contents) {
    do.call(addOptPathEl, c(list(op = op), op.el))
  }
  return(op)
}

listProposals = function(path) {
  list.files(path, pattern = "^prop_[0-9]+_[[:alnum:]]+\\.rds$", full.names = TRUE)
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

readDirectoryToOptState = function(opt.problem, node = 0L) {
  #FIXME: Blocking leads to error in time.budget?
  waitAndBlock(opt.problem, "readDirectoryToOptState", node = node)
  start.time = as.numeric(Sys.time(), units = "secs")
  control = getOptProblemControl(opt.problem)
  opt.path = readOptPathFromDirectory(getAsynDir(opt.problem))
  max.dob = max(getOptPathDOB(opt.path))
  #add proposals with CL or NAs for unevaluated y to opt.path
  readProposalsFromDirectoryToOptPath(opt.path, opt.problem)

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

writeResultToDirectory = function(opt.state, node = 0L) {
  opt.path = getOptStateOptPath(opt.state)
  last.op.el = getOptPathEl(opt.path, getOptPathLength(opt.path))
  prefix = sprintf("state_%.4i_%i_", getOptStateLoop(opt.state), node)
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
  if (isTRUE(ctrl$asyn.cleanup)) {
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


# function.name: function that starts the block
# node: cpu on which the block was started
# thing: eventual data to save
waitAndBlock = function(opt.problem, function.name, node = 0, thing = NULL, time.out = 3000L, wait = 5L) {
  start.time = Sys.time()
  listBlocks = function() {
    list.files(
      getAsynDir(opt.problem), 
      pattern = sprintf("^block_%s_[0-9]+_[[:alnum:]]+\\.rds$", function.name),
      full.names = TRUE
    )
  }
  block.files = listBlocks()
  while (
      getOptProblemControl(opt.problem)$asyn.wait.for.proposals && 
      length(block.files) > 0L && 
      difftime(start.time, Sys.time(), units = "secs") < time.out
    ) {
    block.files = listBlocks()
    Sys.sleep(wait + runif(1, -1, 1))
  }
  block.name = sprintf("block_%s_%i_", function.name, node)
  writeThingToDirectory(opt.problem, thing = thing, prefix = block.name)
}

unblock = function(opt.problem, function.name, node = 0) {
  block.files = list.files(
    getAsynDir(opt.problem), 
    pattern = sprintf("^block_%s_%i_[[:alnum:]]+\\.rds$", function.name, node),
    full.names = TRUE
  )
  lapply(block.files, unlink)
}
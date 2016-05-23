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
  #concept copied from proposePointsConstantLiar.R
  liar = control$multipoint.cl.lie
  lie = liar(getOptPathY(opt.path, control$y.name))
  for (prop.el in file.contents) {
    x = dfRowToList(prop.el$prop.points, getOptProblemParSet(opt.problem), 1)
    dob = max(getOptPathDOB(opt.path))
    last.extra = getOptPathEl(opt.path, getOptPathLength(opt.path))$extra #FIXME: We just cheat and copy last known extras to new lie ¯\_(ツ)_/¯
    last.extra$prop.type = "liar"
    addOptPathEl(opt.path, x = x, y = lie, dob = dob + 1, extra = last.extra) 
  }
}

hashOptPath = function(opt.path) {
  digest::sha1(list(
  getOptPathX(opt.path),
  getOptPathY(opt.path),
  getOptPathDOB(opt.path),
  getOptPathCol(opt.path, "prop.type")  
  ))
}

readDirectoryToOptState = function(opt.problem, respect.block = TRUE, time.out = 60) {
  start.time = as.numeric(Sys.time(), units = "secs")
  control = getOptProblemControl(opt.problem)
  repeat {
    opt.path = readOptPathFromDirectory(getAsynDir(opt.problem))
    max.dob = max(getOptPathDOB(opt.path))
    #add proposals with CL to opt.path
    readProposalsFromDirectoryToOptPath(opt.path, opt.problem)
    #if we do not have to look fore identical opt.paths: break and continue normal
    if (!respect.block) break
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
    Sys.sleep(0.1)
  }
  #find the first available exec.timestamp which does not belong to init.design
  if (max.dob > 0) {
    exec.timestamps = getOptPathCol(opt.path, "exec.timestamp", dob = seq_len(max.dob))
    start.time = min(c(start.time, exec.timestamps), na.rm = TRUE)
  }
  makeOptState(
    opt.problem = opt.problem, 
    opt.path = opt.path, 
    loop = max(getOptPathDOB(opt.path)) + 1,
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
  path = getAsynDir(opt.problem)
  unlink(path, recursive = TRUE, force = TRUE)
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

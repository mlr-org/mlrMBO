readOptPathFromDirectory = function(path) {
  op = readRDS(file.path(path, "state_0_init.rds"))
  files = list.files(path, pattern = "^state_[[:alnum:]]+\\.rds$", full.names = TRUE)
  file.contents = lapply(files, readRDS)
  for (op.el in file.contents) {
    do.call(addOptPathEl, c(list(op = op), op.el))
  }
  return(op)
}

readProposalsFromDirectoryToOptPath = function(opt.path, opt.problem, read.at.least = 0, time.out = 60) {
  control = getOptProblemControl(opt.problem)
  path = getAsynDir(opt.problem)
  start.time = Sys.time()
  repeat {
    files = list.files(path, pattern = "^prop_[[:alnum:]]+\\.rds$", full.names = TRUE)
    if (length(files) >= read.at.least || difftime(Sys.time(), start.time, units = "secs") > read.at.least*time.out) {
      break
    } else {
      Sys.sleep(1)
    }
  }
  file.contents = lapply(files, readRDS)
  #concept copied from proposePointsConstantLiar.R
  liar = control$multipoint.cl.lie
  lie = liar(getOptPathY(opt.path, control$y.name))
  for (prop.el in file.contents) {
    x = dfRowToList(prop.el$prop.points, getOptProblemParSet(opt.problem), 1)
    dob = max(getOptPathDOB(opt.path))
    addOptPathEl(opt.path, x = x, y = lie, dob = dob + 1, extra = getOptPathEl(opt.path, dob)$extra) #FIXME: We just cheat and copy last known extras to new lie ¯\_(ツ)_/¯
  }
}

readDirectoryToOptState = function(opt.problem, proposals.read.at.least = 0) {
  control = getOptProblemControl(opt.problem)
  opt.path = readOptPathFromDirectory(getAsynDir(opt.problem))
  #add proposals with CL to opt.path
  if (control$multipoint.method == "cl") {
    readProposalsFromDirectoryToOptPath(opt.path, opt.problem, read.at.least = proposals.read.at.least)
  }

  #find the first available exec.timestamp which does not belong to init.design
  start.time = as.integer(Sys.time())
  for(i in seq_along(opt.path$env$extra)[getOptPathDOB(opt.path)!=0]) {
    start.time = opt.path$env$extra[[i]]$exec.timestamp
    if (!is.null(start.time) && !is.na(start.time)) break
  }

  makeOptState(
    opt.problem = opt.problem, 
    opt.path = opt.path, 
    loop = sum(getOptPathDOB(opt.path)!=0) + 1,
    time.used = as.integer(Sys.time()) - start.time
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
    hash = substr(digest::sha1(list(Sys.time(), Sys.info())), 1, 64-nchar(prefix)-4) #filenames will have length 64
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

getAsynDir = function(opt.problem) {
  path = file.path(dirname(getOptProblemControl(opt.problem)$save.file.path), "asyn")
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  path
}

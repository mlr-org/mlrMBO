readOptPathFromDirectory = function(path) {
  op = readRDS(file.path(path, "state_0_init.rds"))
  files = list.files(path, pattern = "^state_[[:alnum:]]+\\.rds$", full.names = TRUE)
  file.contents = lapply(files, readRDS)
  for (op.el in file.contents) {
    do.call(addOptPathEl, c(list(op = op), op.el))
  }
  return(op)
}

readProposalsFromDirectoryToOptPath = function(path, opt.path, opt.problem) {
  control = getOptProblemControl(opt.problem)
  path = dirname(control$save.file.path)
  files = list.files(path, pattern = "^prop_[[:alnum:]]+\\.rds$", full.names = TRUE)
  file.contents = lapply(files, readRDS)
  #concept copied from proposePointsConstantLiar.R
  liar = control$multipoint.cl.lie
  lie = liar(getOptPathY(opt.path, control$y.name))
  for (prop.el in file.contents) {
    x = dfRowToList(prop.el$prop.points, getOptProblemParSet(opt.problem), 1)
    addOptPathEl(opt.path, x = x, y = lie, dob = dob)
  }
}

readDirectoryToOptState = function(opt.problem) {
  control = getOptProblemControl(opt.problem)
  opt.path = readOptPathFromDirectory(dirname(control$save.file.path))
  #add proposals with CL to opt.path
  if (control$multipoint.method == "cl") {
    readProposalsFromDirectoryToOptPath(path, opt.path, opt.problem)
  }
  makeOptState(
    opt.problem = opt.problem, 
    opt.path = opt.path, 
    loop = getOptPathLength(opt.path)
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
  path = dirname(getOptProblemControl(opt.problem)$save.file.path)
  save.file = file.path(path, sprintf("%s%s.rds", prefix, hash))
  saveRDS(thing, file = save.file)
  save.file
}
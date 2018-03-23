setOptStateModels = function(opt.state, models) {
  opt.state$models = models
  opt.state$models.loop = getOptStateLoop(opt.state)
  invisible()
}

setOptStateRandomSeed = function(opt.state) {
  opt.state$random.seed = getRandomSeed()
  invisible()
}

setOptStateTasks = function(opt.state, tasks) {
  opt.state$tasks = tasks
  opt.state$tasks.loop = getOptStateLoop(opt.state)
  invisible()
}

setOptStateTimeModel = function(opt.state, time.model) {
  opt.state$time.model = time.model
  invisible()
}

setOptStateLoop = function(opt.state, loop = NULL) {
  if (is.null(loop))
    opt.state$loop = opt.state$loop + 1L
  else
    opt.state$loop = loop
  # save resampling and models in result routine
  setOptStateRandomSeed(opt.state)
  setOptStateTimeUsed(opt.state)
  invisible()
}

setOptStateTimeLastSaved = function(opt.state, time) {
  opt.state$time.last.saved = time
  invisible()
}

setOptStateLoopStarttime = function(opt.state) {
  opt.state$loop.starttime = Sys.time()
  invisible()
}

setOptStateTimeUsed = function(opt.state, time.used = NULL, time.add = NULL) {
  if (!is.null(time.used)) {
    opt.state$time.used = time.used
  } else if (!is.null(time.add)) {
    opt.state$time.used = getOptStateTimeUsed(opt.state) + time.add
  } else {
    opt.state$time.used = getOptStateTimeUsed(opt.state) + difftime(Sys.time(), getOptStateLoopStarttime(opt.state), units = "secs")
    setOptStateLoopStarttime(opt.state)
  }
  invisible()
}

setOptStateState = function(opt.state, state) {
  assertSubset(state, getOptStateValidStates())
  opt.state$state = state
  invisible()
}

setOptStateProgress = function(opt.state, progress) {
  assertNumber(progress, null.ok = TRUE)
  opt.state$progress = progress
  invisible()
}

setOptStateModels = function(opt.state, models) {
  opt.state$models = models
  opt.state$models.loop = getOptStateLoop(opt.state)
  invisible()
}

setOptStateRandomSeed = function(opt.state) {
  opt.state$random.seed = .Random.seed
  invisible()
}

setOptStateTasks = function(opt.state, tasks) {
  opt.state$tasks = tasks
  opt.state$tasks.loop = getOptStateLoop(opt.state)
  invisible()
}

setOptStateTimeModel = function(opt.state, time.model) {
  opt.state$time.model = time.model
}

setOptStateLoop = function(opt.state, loop = NULL) {
  opt.result = getOptStateOptResult(opt.state)
  setOptResultResampleResults(opt.result, opt.state)
  setOptResultStoredModels(opt.result, opt.state)
  if (is.null(loop))
    opt.state$loop = opt.state$loop + 1L
  else
    opt.state$loop = loop
  # save resampling and models in result routine
  setOptStateRandomSeed(opt.state)
  invisible()
}

setOptStateTimeLastSaved = function(opt.state, time) {
  opt.state$time.last.saved = time
  invisible()
}

setOptStateState = function(opt.state, state) {
  assertSubset(state, c("init", "iter", getTerminateChars()))
  opt.state$state = state
  invisible()
}
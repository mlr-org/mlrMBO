# magic mboTemplate - in this function the mbo magic for all our mbo approaches
# does happen - model fitting and point proposal in a generall way. the respective
# mbo algorithms differ in the subfunctions.
# - usually the mboTemplate is started from an OptProblem which is an environment.
# - mboTemplate can also be called from mboContinue from a saved OptState
# - The opt.state is also en environment linking to the main Objects
#    - OptProblem (constant; stores the information which define the problem)
#    - OptPath (stores all information about function evaluations)
#    - OptResult (stores information which should be part of the later constructed mboResult)
#    - (see respective source files for further information)

mboTemplate = function(obj) {
  UseMethod("mboTemplate")
}

# Creates the initial OptState and then runs the template on it
mboTemplate.OptProblem = function(obj) {
  opt.state = makeOptState(obj)
  # evaluate initial design (if y not given) and log to optpath
  evalMBODesign.OptState(opt.state)
  finalizeMboLoop(opt.state)
  mboTemplate(opt.state)
}

# Runs the mbo iterations on any given OptState until termination criterion is fulfilled
mboTemplate.OptState = function(obj) {
  opt.state = obj
  setOptStateLoopStarttime(opt.state)
  # check if budget is already exceeded after intitial design creation
  terminate = getOptStateTermination(opt.state)
  if (terminate$term) {
    opt.problem = getOptStateOptProblem(opt.state)
    showInfo(getOptProblemShowInfo(opt.problem), "%s. The termination conditions
      was satisfied right after the creation of the initial design!", terminate$message)
    return(opt.state)
  }
  repeat {
    prop = proposePoints(opt.state)
    evalProposedPoints.OptState(opt.state, prop)
    finalizeMboLoop(opt.state)
    terminate = getOptStateTermination(opt.state)
    if (terminate$term) {
      break
    }
  }
  opt.state
}

finalizeMboLoop = function(opt.state) {
  # put resampling of surrogate learner and the model itself in the result environment
  opt.result = getOptStateOptResult(opt.state)
  setOptResultResampleResults(opt.result, opt.state)
  setOptResultStoredModels(opt.result, opt.state)
  # Indicate, that we are finished by increasing the loop by one
  setOptStateLoop(opt.state)
  # save on disk routine
  # save with increased loop so we can directly start from here again
  if (getOptStateShouldSave(opt.state))
    saveOptState(opt.state)
  invisible()
}

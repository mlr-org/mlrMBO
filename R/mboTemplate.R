# magic mboTemplate - in this function the mbo magic for all our mbo approaches
# does happen - model fitting und point proposal in a generall way. the respective
# mbo algorithms differ in the subfunctions.
# Everything happens respective to the three objects
# - the OptProblem: See OptProblem.R
# - the OptState: See OptState.R OptState_getter.R OptState_setter.R
# - the OptResult: See OptResult.R
# We cann call the mboTemplate on an OptProblem or continue mboTemplate on a given OptState


mboTemplate = function(obj) {
  UseMethod("mboTemplate")
}

# Creates the initial OptState and runs the template on it
mboTemplate.OptProblem = function(obj) {
  opt.state = makeOptState(obj)
  generateMBODesign.OptState(opt.state)
  setOptStateLoop(opt.state) #loop + 1
  mboTemplate(opt.state)
}

# Runs the mbo iterations on any given OptState until termination criterion is fulfilled
mboTemplate.OptState = function(obj) {
  opt.state = obj
  repeat {
    prop = proposePoints.OptState(opt.state) 
    evalProposedPoints.OptState(opt.state, prop)

    # we are ready with the loop and can count + 1
    # and save everything we used
    setOptStateLoop(opt.state)

    # save on disk routine
    # save with increased loop so we can directly start from here again
    if (getOptStateShouldSave(opt.state))
      saveOptState(opt.state)

    terminate = getOptStateTermination(opt.state)
    if (terminate > 0L)
        break
  } 
  opt.state
}

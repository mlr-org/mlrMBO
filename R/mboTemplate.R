# magic mboTemplate - in this function the mbo magic for all our mbo approaches
# does happen - model fitting und point proposal in a generall way. the respective
# mbo algorithms differ in the subfunctions.

# continue - do we continue an already started (and aborted) mbo run or is this
#            a fresh one?
mboTemplate = function(obj) {
  UseMethod("mboTemplate")
}

mboTemplate.OptProblem = function(obj) {
  opt.state = makeOptState(obj)
  generateMBODesign.OptState(opt.state)
  setOptStateLoop(opt.state) #loop + 1
  mboTemplate(opt.state)
}

mboTemplate.OptState = function(obj) {
  #we start with a tuning state in the loop we want it to be
  #loop = 1 is default
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

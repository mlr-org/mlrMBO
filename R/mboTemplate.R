# magic mboTemplate - in this function the mbo magic for all our mbo approaches
# does happen - model fitting und point proposal in a generall way. the respective
# mbo algorithms differ in the subfunctions.

# continue - do we continue an already started (and aborted) mbo run or is this
#            a fresh one?
mboTemplate = function(obj) {
  UseMethod("mboTemplate")
}

mboTemplate.TuningProblem = function(obj) {
  tuningState = makeTuningState(obj)
  generateMBODesign.TuningState(tuningState)
  setTuningStateLoop(tuningState) #loop + 1
  mboTemplate(tuningState)
}

mboTemplate.TuningState = function(obj) {
  #we start with a tuning state in the loop we want it to be
  #loop = 1 is default
  tuningState = obj
  repeat {
    prop = proposePoints.TuningState(tuningState) 
    evalProposedPoints.TuningState(tuningState, prop)

    # we are ready with the loop and can count + 1
    # and save everything we used
    setTuningStateLoop(tuningState)

    # save on disk routine
    # save with increased loop so we can directly start from here again
    if (getTuningStateShouldSave(tuningState))
      saveTuningState(tuningState)
    terminate = getTuningStateTermination(tuningState)
    if (terminate >= 0)
        break
  } 
  tuningState
}

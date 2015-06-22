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
  setTuningStateLoop(tuningState) #loop + 1 and invalidate old
  mboTemplate(tuningState)
}

mboTemplate.TuningState = function(obj) {
  tuningState = obj

  repeat {
      prop = proposePoints.TuningState(tuningState) 
      evalProposedPoints.TuningState(tuningState, prop)
      setTuningStateLoop(tuningState) #loop + 1 and invalidate old models
      terminate = getTuningStateTermination(tuningState)
      if (terminate >= 0)
        break
    }

    #make sure to invalidate models at end
    setTuningStateLoop(tuningState)
    
    # make sure to save final res on disk
    saveTuningStateNow(tuningState)

    tuningState
}

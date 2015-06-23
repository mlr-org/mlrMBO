makeTuningProblem = function(fun, par.set, design = NULL, learner, control, start.time = NULL, show.info = TRUE, more.args = list()) {
  tuningProblem = new.env()

  tuningProblem$fun = fun
  tuningProblem$par.set = par.set
  tuningProblem$design = design
  tuningProblem$learner = learner
  tuningProblem$control = control
  if (is.null(start.time))
    tuningProblem$start.time = Sys.time()
  else
    tuningProblem$start.time = start.time
  tuningProblem$show.info = show.info
  tuningProblem$more.args = more.args

  #save old options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )
  tuningProblem$oldopts = oldopts

  tuningProblem$algo.init = NULL

  class(tuningProblem) = append(class(tuningProblem), "TuningProblem")

  tuningProblem
}

getTuningProblemFun = function(tuningProblem) {
  tuningProblem$fun
}

getTuningProblemLearner = function(tuningProblem) {
  tuningProblem$learner
}

getTuningProblemControl = function(tuningProblem) {
  tuningProblem$control
}

getTuningProblemParSet = function(tuningProblem) {
  tuningProblem$par.set
}

getTuningProblemOldopts = function(tuningProblem) {
  tuningProblem$oldopts
}

getTuningProblemMoreArgs = function(tuningProblem) {
  tuningProblem$more.args
}

setTuningProblemAlgoInit = function(tuningProblem, algo.init) {
  tuningProblem$algo.init = algo.init
  invisible()
}

getTuningProblemAlgoInit = function(tuningProblem) {
  par.set = getTuningProblemParSet(tuningProblem)
  control = getTuningProblemControl(tuningProblem)
  if (is.null(tuningProblem$algo.init)) {
    algo.init = mboAlgoInit(par.set = par.set, control = control)
    setTuningProblemAlgoInit(tuningProblem, algo.init)
  } else {
    algo.init = tuningProblem$algo.init
  }
  algo.init
}

getTuningProblemStartTime = function(tuningProblem) {
  tuningProblem$start.time
}

getTuningProblemShowInfo = function(tuningProblem) {
  tuningProblem$show.info
}

setTuningProblemDesign = function(tuningProblem, design) {
  tuningProblem$design = design
  invisible()
}

getTuningProblemDesign = function(tuningProblem) {
  if (is.null(tuningProblem$design)) {
    #design = generateMBODesign(tuninProblem) #FIXME implement?
    design = NULL
    #setTuningProblemDesign(tuningProblem, design)
  } else {
    design = tuningProblem$design
  }
  design
}

getTuningProblemInitDesignPoints = function(tuningProblem) {
  design = getTuningProblemDesign(tuningProblem)
  if (is.null(design)) 
    getTuningProblemControl(tuningProblem)$init.design.points
  else
    nrow(design)
}

getTuningProblemIsLcb = function(tuningProblem) {
  control = getTuningProblemControl(tuningProblem)
  control$propose.points > 1L && control$multipoint.method == "lcb"
}

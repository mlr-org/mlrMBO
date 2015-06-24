##' The TuningProblem contains all the constants wich define a TuningProblem within our MBO Steps. It is an enviroment and is always pointed at by the TuningState.
##' @param fun [\code{function(x, ...)}]\cr
##'   Fitness function to minimize. The first argument has to be a list of values.
##'   The function has to return a single numerical value.
##'   In fact it is possible to return even more information which will be stored
##'   in the optimization path. To achieve this, simply append the attribute \dQuote{extras}
##'   to the return value of the target function. This has to be a named list of scalar values.
##'   Each of these values will be stored additionally in the optimization path.
##' @template arg_parset
##' @param design [\code{data.frame} | NULL]\cr
##'   Initial design as data frame.
##'   If the parameters have corresponding trafo functions,
##'   the design must not be transformed before it is passed!
##'   If \code{NULL}, one is constructed from the settings in \code{control}.
##' @param learner [\code{\link[mlr]{Learner}}]\cr
##'   Regression learner to model \code{fun}.
##' @template arg_control
##' @arg start.time [\code{POSIXct}] \cr
##'   Starting time of Tuning defined as an output from \code[Sys.time()].
##' @template arg_showinfo
##' @param more.args [list]\cr
##'   Further arguments passed to fitness function.
##' @return [\code{\link{MBOSingleObjResult}} | \code{\link{MBOMultiObjResult}}]
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

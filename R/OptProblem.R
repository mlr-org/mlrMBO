#' @title OptProblem object.
#' @description
#' The OptProblem contains all the constants values wich define a OptProblem within our MBO Steps.
#' It is an enviroment and is always pointed at by the OptState.
#' @name OptProblem
#' @rdname OptProblem
NULL

# @param fun [\code{function(x, ...)}]\cr
#   Fitness function to minimize. The first argument has to be a list of values.
#   The function has to return a single numerical value.
#   In fact it is possible to return even more information which will be stored
#   in the optimization path. To achieve this, simply append the attribute \dQuote{extras}
#   to the return value of the target function. This has to be a named list of scalar values.
#   Each of these values will be stored additionally in the optimization path.
# @template arg_parset
# @param design [\code{data.frame} | NULL]\cr
#   Initial design as data frame.
#   If the parameters have corresponding trafo functions,
#   the design must not be transformed before it is passed!
#   If \code{NULL}, one is constructed from the settings in \code{control}.
# @param learner [\code{\link[mlr]{Learner}}]\cr
#   Regression learner to model \code{fun}.
# @template arg_control
# @template arg_showinfo
# @param more.args [list]\cr
#   Further arguments passed to fitness function.
# @return [\code{\link{MBOSingleObjResult}} | \code{\link{MBOMultiObjResult}}]
makeOptProblem = function(fun, par.set, design = NULL, learner, control, show.info = TRUE, more.args = list()) {
  opt.problem = new.env()

  opt.problem$fun = fun
  opt.problem$par.set = par.set
  opt.problem$design = design
  opt.problem$learner = learner
  opt.problem$control = control
  opt.problem$show.info = show.info
  opt.problem$more.args = more.args

  #save old options
  oldopts = list(
    ole = getOption("mlr.on.learner.error"),
    slo = getOption("mlr.show.learner.output")
  )
  opt.problem$oldopts = oldopts

  opt.problem$algo.init = NULL

  class(opt.problem) = append(class(opt.problem), "OptProblem")

  opt.problem
}

getOptProblemFun = function(opt.problem) {
  opt.problem$fun
}

getOptProblemLearner = function(opt.problem) {
  opt.problem$learner
}

getOptProblemTimeLearner = function(opt.problem) {
  makeTrafoWrapper(opt.problem$learner, log, exp)
}

getOptProblemControl = function(opt.problem) {
  opt.problem$control
}

getOptProblemParSet = function(opt.problem) {
  opt.problem$par.set
}

getOptProblemOldopts = function(opt.problem) {
  opt.problem$oldopts
}

getOptProblemMoreArgs = function(opt.problem) {
  opt.problem$more.args
}

setOptProblemAlgoInit = function(opt.problem, algo.init) {
  opt.problem$algo.init = algo.init
  invisible()
}

getOptProblemAlgoInit = function(opt.problem) {
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  if (is.null(opt.problem$algo.init)) {
    algo.init = mboAlgoInit(par.set = par.set, control = control)
    setOptProblemAlgoInit(opt.problem, algo.init)
  } else {
    algo.init = opt.problem$algo.init
  }
  algo.init
}

getOptProblemShowInfo = function(opt.problem) {
  opt.problem$show.info
}

setOptProblemDesign = function(opt.problem, design) {
  opt.problem$design = design
  invisible()
}

getOptProblemDesign = function(opt.problem) {
  if (is.null(opt.problem$design)) {
    #design = generateMBODesign(tuninProblem) #FIXME implement?
    design = NULL
    #setOptProblemDesign(opt.problem, design)
  } else {
    design = opt.problem$design
  }
  design
}

getOptProblemInitDesignPoints = function(opt.problem) {
  design = getOptProblemDesign(opt.problem)
  if (is.null(design)) 
    getOptProblemControl(opt.problem)$init.design.points
  else
    nrow(design)
}

getOptProblemIsLcb = function(opt.problem) {
  control = getOptProblemControl(opt.problem)
  control$propose.points > 1L && control$multipoint.method == "lcb"
}

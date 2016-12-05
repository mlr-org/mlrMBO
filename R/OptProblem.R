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

# IMPORTANT NOTE:
# See this as a constructor and it's variables as member variables.
# All variables in this Object should be documented here.
# Think of it, when you implement new ones!
# Unfortunately in R we cannot hinder you from putting other values in this object, but please: Don't!
makeOptProblem = function(fun, design = NULL, learner, control, show.info = TRUE, more.args = list()) {
  opt.problem = new.env()

  opt.problem$fun = fun
  opt.problem$design = design
  opt.problem$learner = learner
  opt.problem$control = control
  opt.problem$show.info = show.info
  opt.problem$more.args = more.args
  opt.problem$all.possible.weights = NULL

  class(opt.problem) = append(class(opt.problem), "OptProblem")

  opt.problem
}

getOptProblemFun = function(opt.problem) {
  opt.problem$fun
}

getOptProblemLearner = function(opt.problem) {
  opt.problem$learner
}

getOptProblemControl = function(opt.problem) {
  opt.problem$control
}

getOptProblemParSet = function(opt.problem) {
  getParamSet(getOptProblemFun(opt.problem))
}

getOptProblemMoreArgs = function(opt.problem) {
  opt.problem$more.args
}

setOptProblemAllPossibleWeights = function(opt.problem, all.possible.weights) {
  opt.problem$all.possible.weights = all.possible.weights
  invisible()
}

getOptProblemAllPossibleWeights = function(opt.problem) {
  control = getOptProblemControl(opt.problem)
  if (is.null(opt.problem$all.possible.weights) && control$n.objectives > 1L && control$multiobj.method == "parego") {
    # calculate all possible weight vectors and save them
    all.possible.weights = combWithSum(control$multiobj.parego.s, control$n.objectives) / control$multiobj.parego.s
    # rearrange them a bit - we want to have the margin weights on top of the matrix
    # tricky: all margin weights have maximal variance
    vars = apply(all.possible.weights, 1, var)
    all.possible.weights = rbind(diag(control$n.objectives), all.possible.weights[!vars == max(vars),])
    setOptProblemAllPossibleWeights(opt.problem, all.possible.weights)
  } else {
    all.possible.weights = opt.problem$all.possible.weights
  }
  all.possible.weights
}

getOptProblemShowInfo = function(opt.problem) {
  opt.problem$show.info
}

setOptProblemDesign = function(opt.problem, design) {
  opt.problem$design = design
  invisible()
}

getOptProblemDesign = function(opt.problem) {
  opt.problem$design
}

print.OptProblem = function(x, ...) {
  catf("OptProblem")
  catf("Objective Function: %s", getName(getOptProblemFun(x)))
  catf("Surrogate Learner:")
  print(getOptProblemLearner(x))
} 

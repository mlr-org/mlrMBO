# Helper to check whether the user selected valid options and combinations of options.
#
# @param fun [\code{function}]
#   Objective fun
# @param par.set [\code{\link[ParamHelpers]{ParamSet}}]
#   Collection of parameters and their constraints for optimization.
# @param design
#   Sampling plan.
# @param learner [\code{\link[mlr]{Learner}}]
#   Learner object.
# @param control [\code{\link{MBOControl}}]
#   MBO control object.
checkStuff = function(fun, par.set, design, learner, control) {
  assertFunction(fun)
  assertClass(par.set, "ParamSet")
  assertClass(control, "MBOControl")
  assertClass(learner, "Learner")

  pids = getParamIds(par.set)

  # check params + learner
  if (any(sapply(par.set$pars, inherits, what = "LearnerParam")))
    stop("No parameter can be of class 'LearnerParam'! Use basic parameters instead to describe you region of interest!")

  if (!hasFiniteBoxConstraints(par.set))
    stop("mbo requires finite box constraints!")

  if (hasDiscrete(par.set) && !hasProperties(learner, "factors"))
    stop("Provided learner does not support factor parameters.")
  
  if (learner$type != "regr")
    stop("mbo requires regression learner!")

  # general infill stuff. relavant for single obj and parego
  if (control$infill.crit %in% c("ei", "aei", "lcb") && learner$predict.type != "se") {
    stopf("For infill criterion '%s' predict.type of learner %s must be set to 'se'!%s",
      control$infill.crit, learner$id,
      ifelse(hasProperties(learner, "se"), "",
        "\nBut this learner does not seem to support prediction of standard errors! You could use the mlr wrapper makeBaggingWrapper to bootstrap the standard error estimator."))
  }

  if (control$infill.opt %in% c("cmaes", "ea") && !isNumeric(par.set))
    stopf("Optimizer '%s' can only be applied to numeric, integer, numericvector, integervector parameters!", control$infill.opt)

  # For now allow constant liar only in combinaton with Kriging
  # (see https://github.com/berndbischl/mlrMBO/issues/12)
  if (control$multipoint.method == "cl" && learner$id != "regr.km")
    stop("Constant liar can currently only be used with Kriging surrograte.")

  #  single objective
  if (control$number.of.targets == 1L) {
    if (control$propose.points == 1L) { # single point
    } else {                            # multi point
      if ((control$multipoint.method %in% c("lcb", "cl", "lcb") ||
          control$multipoint.method == "multicrit" && control$multipoint.multicrit.objective != "mean.dist" )
        && learner$predict.type != "se") {
        stopf("For multipoint method '%s'%s, predict.type of learner %s must be set to 'se'!%s",
          control$multipoint.method,
          ifelse(control$multipoint.method == "multicrit",
            sprintf(" with objective '%s'", control$multipoint.multicrit.obj), ""),
          learner$id,
          ifelse(hasProperties(learner, "se"), "",
            "\nBut this learner does not support prediction of standard errors!"))
      }
    }
  }

  # for EI we need mu + sd
  #  if (control$infill.opt == "EI" &&
  #          !(class(learner) %in% c("regr.km", "regr.kmforrester")))
  #      stop("Expected improvement can currently only be used with learner 'regr.km' and 'regr.kmforrester'!")
  # if (!(inherits(learner, "regr.randomForest") || inherits(learner, "regr.rpart")) && hasRequires(par.set))
  #   stop("Parameter sets with dependent parameters currently require the learner to be a randomForest or rpart.")

  ##### multifid #####
  if (control$multifid) {
    if (control$multifid.param %nin% pids)
      stopf("Fidelity param '%s' must be in par.set!", control$multifid.param)
  }
}

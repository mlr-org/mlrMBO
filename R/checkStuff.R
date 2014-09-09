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

  # general parameter and learner checks
  if (any(sapply(par.set$pars, inherits, what = "LearnerParam")))
    stop("No parameter can be of class 'LearnerParam'! Use basic parameters instead to describe you region of interest!")

  if (!hasFiniteBoxConstraints(par.set))
    stop("mbo requires finite box constraints!")

  if (hasDiscrete(par.set) && !hasProperties(learner, "factors"))
    stop("Provided learner does not support factor parameters.")

  if (learner$type != "regr")
    stop("mbo requires regression learner!")

  # general infill stuff (relavant for single objective and parEGO)
  if (control$infill.crit %in% c("se", "ei", "aei", "lcb", "sms") && learner$predict.type != "se") {
    stopf("For infill criterion '%s' predict.type of learner %s must be set to 'se'!%s",
      control$infill.crit, learner$id,
      ifelse(hasProperties(learner, "se"), "",
        "\nBut this learner does not seem to support prediction of standard errors! You could use the mlr wrapper makeBaggingWrapper to bootstrap the standard error estimator."))
  }

  # compatibility of optimizers and parameters
  if (control$infill.opt %in% c("cmaes", "ea") && !isNumeric(par.set))
    stopf("Optimizer '%s' can only be applied to numeric, integer, numericvector, integervector parameters!", control$infill.opt)

  # For now allow constant liar only in combinaton with kriging
  if (control$multipoint.method == "cl" && learner$predict.type != "se") {
    stopf("For multipoint method constant liar (cl) predict.type of learner %s must be set to 'se'!%s",
      learner$id,
      ifelse(hasProperties(learner, "se"), "",
        "\nBut this learner does not seem to support prediction of standard errors! You could use the mlr wrapper makeBaggingWrapper to bootstrap the standard error estimator."))
  }

  if (control$multipoint.method == "cl" && control$infill.crit != "ei") {
    stopf("Multipoint proposal using constant liar needs the infill criterion to 'ei' (expected improvement), but your selection is '%s'!", control$infill.crit)
  }

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

  # multicrit stuff
  if (control$multicrit.method == "sms" && control$infill.crit %nin% c("sms", "eps"))
    stopf("For multicrit 'sms' infil.crit must be set to 'sms' or 'eps'!")
  
  if (control$multicrit.method == "mspot" && control$infill.opt != "nsga2")
    stopf("For multicrit 'mspot' infil.opt must be set to 'nsga2'!")

  # multifidelity stuff
  if (control$multifid) {
    if (control$multifid.param %nin% pids)
      stopf("Fidelity param '%s' must be in par.set!", control$multifid.param)
  }
}

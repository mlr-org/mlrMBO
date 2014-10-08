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
  if (!is.null(design))
    assertClass(design, "data.frame")
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

  if (hasRequires(par.set) && !hasProperties(learner, "missings"))
    stopf("The 'par.set' has dependent parameters, which will lead to missing values in X-space during modeling, but learner '%s' does not support handling of missing values (property 'missing')!", learner$id)


  # general infill stuff (relavant for single objective and parEGO)
  if (control$infill.crit %in% c("se", "ei", "aei", "lcb", "dib") && learner$predict.type != "se") {
    stopf("For infill criterion '%s' predict.type of learner %s must be set to 'se'!%s",
      control$infill.crit, learner$id,
      ifelse(hasProperties(learner, "se"), "",
        "\nBut this learner does not seem to support prediction of standard errors! You could use the mlr wrapper makeBaggingWrapper to bootstrap the standard error estimator."))
  }

  # compatibility of optimizers and parameters
  if (control$infill.opt %in% c("cmaes", "ea") && !isNumeric(par.set))
    stopf("Optimizer '%s' can only be applied to numeric, integer, numericvector, integervector parameters!", control$infill.opt)

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
      if (control$multipoint.method == "cl" && control$infill.crit != "ei")
        stopf("Multipoint proposal using constant liar needs the infill criterion 'ei' (expected improvement), but you used '%s'!", control$infill.crit)
      if (control$multipoint.method == "lcb" && control$infill.crit != "lcb")
        stopf("Multipoint proposal using parallel LCB needs the infill criterion 'lcb' (lower confidence bound), but you used '%s'!", control$infill.crit)
    }
  }

  # multicrit stuff
  if (control$number.of.targets > 1L) {
    if (control$multicrit.method == "dib") {
      if (control$infill.crit != "dib")
        stopf("For multicrit 'dib' infil.crit must be set to 'dib'!")
    } else {
      if (control$infill.crit == "dib")
        stopf("For infill.crit 'dib', multicrit method 'dib' is needed!")
    }
    if (control$multicrit.method == "mspot" && control$infill.opt != "nsga2")
      stopf("For multicrit 'mspot' infil.opt must be set to 'nsga2'!")
  }

  # multifidelity stuff
  if (control$multifid) {
    if (control$multifid.param %nin% pids)
      stopf("Fidelity param '%s' must be in par.set!", control$multifid.param)
  }

  # propose point filtering
  # FIXME: implement something that works for integer and discrte params
  if (control$filter.proposed.points && hasDiscrete(par.set))
    stop("Filtering proposed points currently not implemented for discrete parameters!")
}

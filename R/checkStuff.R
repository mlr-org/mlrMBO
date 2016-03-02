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

  if (getNumberOfObjectives(fun) != control$n.objectives) {
    stopf("Objective function has %i objectives, but the control object assumes %i.",
      getNumberOfObjectives(fun), control$n.objectives)
  }

  # at the moment we do not support noisy multicriteria optimization
  if (getNumberOfObjectives(fun) > 1L && isNoisy(fun)) {
    stopf("Optimization of noisy multi-objective functions not supported in the moment.")
  }

  # general parameter and learner checks
  if (any(vlapply(par.set$pars, inherits, what = "LearnerParam")))
    stop("No parameter can be of class 'LearnerParam'! Use basic parameters instead to describe you region of interest!")

  if (!hasFiniteBoxConstraints(par.set))
    stop("mbo requires finite box constraints!")

  if (hasDiscrete(par.set) && !hasLearnerProperties(learner, "factors"))
    stop("Provided learner does not support factor parameters.")

  if (learner$type != "regr")
    stop("mbo requires regression learner!")

  if (hasRequires(par.set) && !hasLearnerProperties(learner, "missings"))
    stopf("The 'par.set' has dependent parameters, which will lead to missing values in X-space during modeling, but learner '%s' does not support handling of missing values (property 'missing')!", learner$id)

  # general infill stuff (relavant for single objective and parEGO)
  if (control$infill.crit %in% c("se", "ei", "aei", "cb", "dib") && learner$predict.type != "se") {
    stopf("For infill criterion '%s' predict.type of learner %s must be set to 'se'!%s",
      control$infill.crit, learner$id,
      ifelse(hasLearnerProperties(learner, "se"), "",
        "\nBut this learner does not seem to support prediction of standard errors! You could use the mlr wrapper makeBaggingWrapper to bootstrap the standard error estimator."))
  }

  # If nugget estimation should be used, make sure learner is a km model with activated nugget estim
  if (control$infill.crit.aei.use.nugget) {
    if (learner$short.name != "km" || !isTRUE(getHyperPars(learner)$nugget.estim)) {
      stop("You have to turn on nugget estimation in your Kriging Model, if you want to use nugget estimation in the aei!")
    }
  }
  # compatibility of optimizers and parameters
  if (control$infill.opt %in% c("cmaes", "ea") && !isNumeric(par.set))
    stopf("Optimizer '%s' can only be applied to numeric, integer, numericvector, integervector parameters!", control$infill.opt)

  # set default mu parameter for ea infill optimizer
  if (control$infill.opt == "ea") {
    control$infill.opt.ea.mu = coalesce(control$infill.opt.ea.mu, getNumberOfParameters(fun) * 10L)
    assertNumber(control$infill.opt.ea.mu, na.ok = FALSE, lower = 0)
  }

  if (is.null(control$target.fun.value)) {
    # If we minimize, target is -Inf, for maximize it is Inf
    control$target.fun.value = if (control$minimize[1L]) -Inf else Inf
  } else {
    assertNumber(control$target.fun.value, na.ok = FALSE)
  }

  if (length(control$save.on.disk.at) > 0L && (control$iters + 1) %nin% control$save.on.disk.at)
    warningf("You turned off the final saving of the optimization result at (iter + 1)! Do you really want this?")
  if (length(control$save.on.disk.at) > 0 || is.finite(control$save.on.disk.at.time)) {
    control$save.on.disk.at = asInteger(control$save.on.disk.at, any.missing = FALSE, lower = 0 , upper = control$iters + 1)
    assertPathForOutput(control$save.file.path)
  }
  control$store.model.at = coalesce(control$store.model.at, control$iters + 1)
  control$resample.at = coalesce(control$resample.at, integer(0))

  #  single objective
  if (control$n.objectives == 1L) {
    if (control$propose.points == 1L) { # single point
    } else {                            # multi point
      if ((control$multipoint.method %in% c("cb", "cl", "cb") ||
          control$multipoint.method == "multicrit" && control$multipoint.multicrit.objective != "mean.dist" )
        && learner$predict.type != "se") {
        stopf("For multipoint method '%s'%s, predict.type of learner %s must be set to 'se'!%s",
          control$multipoint.method,
          ifelse(control$multipoint.method == "multicrit",
            sprintf(" with objective '%s'", control$multipoint.multicrit.obj), ""),
          learner$id,
          ifelse(hasLearnerProperties(learner, "se"), "",
            "\nBut this learner does not support prediction of standard errors!"))
      }
      if (control$multipoint.method == "cl" && control$infill.crit != "ei")
        stopf("Multipoint proposal using constant liar needs the infill criterion 'ei' (expected improvement), but you used '%s'!", control$infill.crit)
      if (control$multipoint.method == "cb" && control$infill.crit != "cb")
        stopf("Multipoint proposal using parallel cb needs the infill criterion 'cb' (confidence bound), but you used '%s'!", control$infill.crit)
    }
  }

  # multicrit stuff
  if (control$n.objectives > 1L) {
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

  # propose point filtering
  # FIXME: implement something that works for integer and discrte params
  if (control$filter.proposed.points && hasDiscrete(par.set))
    stop("Filtering proposed points currently not implemented for discrete parameters!")
  return(control)
}

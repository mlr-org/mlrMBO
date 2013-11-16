# FIXME: add a correct check for dependent params / model
# FIXME:  check whether model (and settings), ctriteria, optimizer, noisy
# work together
# check whether the user selected valid options / combinations
#FIXME: check whether stuff can be used for factor variables
checkStuff = function(fun, par.set, design, learner, control) {
  checkArg(fun, "function")
  if(any(sapply(par.set$pars, function(x) inherits(x, "LearnerParam"))))
    stop("No par.set parameter in 'mbo' can be of class 'LearnerParam'! Use basic parameters instead to describe you region of interest!")
  if (any(is.infinite(c(getLower(par.set), getUpper(par.set)))))
    stop("mbo requires finite box constraints!")
  if (control$infill.opt == "cmaes" &&
      !all(sapply(par.set$pars, function(p) p$type) %in% c("numeric", "integer", "numericvector", "integervector")))
    stop("Optimizer CMAES can only be applied to numeric, integer, numericvector, integervector parameters!")
  if (learner$type != "regr")
    stop("mbo requires regression learner!")
  if (control$propose.points == 1L) {
    if(control$infill.crit %in% c("ei", "aei", "lcb") && learner$predict.type != "se")
      stopf("For infill criterion '%s' predict.type of learner %s must be set to 'se'!%s",
        control$infill.crit, learner$id,
        ifelse(learner$se, "",
          "\nBut this learner does not seem to support prediction of standard errors!"))
  } else { # multipoint proposal
    if ((control$multipoint.method %in% c("lcb", "cl", "lcb") ||
         control$multipoint.method == "multicrit" && control$multipoint.multicrit.objective != "mean.dist" )
      && learner$predict.type != "se") {
      stopf("For multipoint method '%s'%s, predict.type of learner %s must be set to 'se'!%s",
        control$multipoint.method,
        ifelse(control$multipoint.method == "multicrit", 
          sprintf(" with objective '%s'", control$multipoint.multicrit.obj), ""),
        learner$id,
        ifelse(learner$se, "",
          "\nBut this learner does not seem to support prediction of standard errors!"))
    }
  }
    # for EI we need mu + sd
  #  if (control$infill.opt == "EI" &&
  #          !(class(learner) %in% c("regr.km", "regr.kmforrester")))
  #      stop("Expected improvement can currently only be used with learner 'regr.km' and 'regr.kmforrester'!")
  # if (!(inherits(learner, "regr.randomForest") || inherits(learner, "regr.rpart")) && hasRequires(par.set))
  #   stop("Parameter sets with dependent parameters currently require the learner to be a randomForest or rpart.")
}

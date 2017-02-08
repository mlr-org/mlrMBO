# initial infill criterions with setting specific defaults
initializeCriterion = function(crit, par.set, design, learner, control) {
  UseMethod("initializeCriterion")
}

initializeCriterion.default = function(crit, par.set, design, learner, control) {
  return(crit)
}

initializeCriterion.InfillCritcb = function(crit, par.set, design, learner, control) {
  cb.lambda = crit$cb.lambda
  if (is.null(cb.lambda))
    cb.lambda = ifelse(isSimpleNumeric(par.set), 1, 2)
  makeMBOInfillCriterionCB(cb.lambda)
}
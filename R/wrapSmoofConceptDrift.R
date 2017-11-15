#' @title Wrap a smoof function for concept drift.
#'
#' @description
#' Wrap a smoof function for concept drift.
#'
#' @param fun [\code{smoof_function}]\cr
#'   The smoof function that you want to wrap.
#' @param drift.param [\code{character(1)}]\cr
#'   The parameter that defines the drift.
#' @return return_type
#' @export
wrapSmoofConceptDrift = function(fn, drift.param) {
  assertClass(fn, "smoof_function")
  par.set = getParamSet(fn)
  if (isVector(par.set)) {
    ps.length = getParamLengths(par.set)
    if (length(ps.length)>1) {
      stop("Vector ParamSet with more then one VectorParam are not supported!")
    }
    par = par.set$pars[[1]]
    if (par$type != "numericvector") {
      stop ("Only numericvector params are supported")
    }
    par.ids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
    pars = Map(makeNumericParam, lower = par$lower, upper = par$upper, id = par.ids)
    par.set$pars = setNames(pars, par.ids)
  }
  assertChoice(drift.param, getParamIds(par.set))
  sub.par.set = dropParams(par.set, drift.param)
  assertClass(sub.par.set, "ParamSet")
  fn = setAttribute(fn, "original.par.set", value = par.set)
  fn = setAttribute(fn, "original.global.opt.params", attr(fn, "global.opt.params"))
  fn = setAttribute(fn, "original.global.opt.value", attr(fn, "global.opt.value"))
  fn = setAttribute(fn, "par.set", value = sub.par.set)
  fn = setAttribute(fn, "global.opt.params", value = NULL)
  fn = setAttribute(fn, "original.global.opt.value", value = NULL)
  fn = setAttribute(fn, "drift.param", value = drift.param)
  return(fn)
}
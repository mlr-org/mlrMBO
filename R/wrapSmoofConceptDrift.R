#' @title Wrap a smoof function for concept drift.
#'
#' @description
#' Wrap a smoof function for concept drift.
#'
#' @param fn [\code{smoof_function}]\cr
#'   The smoof function that you want to wrap.
#' @param drift.param [\code{character(1)}]\cr
#'   The parameter that defines the drift.
#' @return return_type
#' @export
wrapSmoofConceptDrift = function(fn, drift.param) {
  assertClass(fn, "smoof_function")
  par.set = getParamSet(fn)
  if (isTRUE(par.set$pars[[drift.param]]$type == "discrete")) {
    fn = wrapSmoofConceptDriftDiscrete(fn, drift.param)
    wrapSmoofConceptDriftNumeric(fn, drift.param)
  } else {
    wrapSmoofConceptDriftNumeric(fn, drift.param)
  }
}

wrapSmoofConceptDriftDiscrete = function(fn, drift.param) {
  par.set = getParamSet(fn)
  disc.values = getValues(par.set$pars[[drift.param]])
  num.replacement.par = makeNumericParam(id = drift.param, lower = 1, upper = length(disc.values) + 1)
  par.set.wrap = par.set
  par.set.wrap$pars[[drift.param]] = num.replacement.par
  stopifnot("x" %in% formalArgs(fn))
  fun.wrap = function(x) {
    disc.ind = min(floor(x[[drift.param]]), length(disc.values))
    x[[drift.param]] = disc.values[[disc.ind]]
    fn(x)
  }
  attributes(fun.wrap) = attributes(fun)
  fun.wrap = setAttribute(fun.wrap, "par.set", value = par.set.wrap)
  fun.wrap
}

wrapSmoofConceptDriftNumeric = function(fn, drift.param) {
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
  fn = setAttribute(fn, "global.opt.value", value = NULL)
  fn = setAttribute(fn, "drift.param", value = drift.param)
  return(fn)
}

unwrapSmoofConceptDriftNumeric = function(fn) {
  attrs = attributes(fn)
  attr.names = names(attrs)
  attr.names$drift.param = NULL
  original.values.inds = grepl("^original\\.", attr.names)
  original.attrs = attrs[original.values.inds]
  names(original.attrs) = sub("^original\\.", "", names(original.attrs))
  attrs = insert(attrs, original.attrs)
  attributes(fn) = attrs
  return(fn)
}

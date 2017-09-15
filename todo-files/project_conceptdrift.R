library(mlrMBO)
library(ggplot2)
fn = makeBraninFunction()
autoplot(fn, render.levels = TRUE)

# we will take x1 as a time factor and optimize over x2

# questions
# For the intial design, the concept does not change?

wrapSmoofConceptDrift = function(fn, drift.param, sub.par.set = NULL) {
  par.set = getParamSet(fn)
  if (drift.param %in% getParamIds(par.set) && is.null(sub.par.set)) {
    sub.par.set = dropNamed(par.set, drift.param)
  }
  fn = setAttribute(fn, "original.par.set", value = par.set)
  fn = setAttribute(fn, "original.global.opt.params", attr(fn, "global.opt.params"))
  fn = setAttribute(fn, "original.global.opt.value", attr(fn, "global.opt.value"))
  fn = setAttribute(fn, "par.set", value = sub.par.set)
  fn = setAttribute(fn, "global.opt.params", value = NULL)
  fn = setAttribute(fn, "original.global.opt.value", value = NULL)
  fn = setAttribute(fn, "drift.param", value = drift.param)
  return(fn)
}

w.fn = wrapSmoofConceptDrift(fn = fn, drift.param = "x1", sub.par.set = makeNumericParamSet(id = "x2", len = 1, lower = 0, upper = 15))

slow.drift = function(dob) {
  -5 + 0.5 * dob
}

ctrl = makeMBOControl()
ctrl = setMBOControllConceptDrift(
  control = ctrl,
  drift.param = "x1",
  drift.function = slow.drift,
  window.width = 10)

ps = getParamSet(fn)
dropNamed(ps, "x1")


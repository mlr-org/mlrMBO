mydev()
library(ggplot2)
fn = makeAlpine02Function(2)
autoplot(fn, render.levels = TRUE)

# we will take x1 as a time factor and optimize over x2

# questions
# For the intial design, the concept does not change?
w.fn = wrapSmoofConceptDrift(fn = fn, drift.param = "x1")

mbo.iters = 60
start.drift = 0.2
drift.range = unlist(attr(w.fn, "original.par.set")$pars[[attr(w.fn, "drift.param")]][c("lower", "upper")])
drift.range = drift.range + c(start.drift, 0)

slow.drift = function(dob) {
  drift.range[1] + (dob/mbo.iters) * diff(drift.range)
}

tail.window = function(x) {
  tail(x, 20)
}

ctrl = makeMBOControl(final.method = "best.predicted")
ctrl = setMBOControlConceptDrift(
  control = ctrl,
  drift.function = slow.drift,
  window.function = tail.window,
  calculate.th.final.point = TRUE
)
ctrl = setMBOControlTermination(ctrl, iter = mbo.iters)

res = mbo(fun = w.fn, control = ctrl)

res$opt.path$window.function = identity
as.data.frame(res$opt.path)
g = autoplot(fn, render.levels = TRUE, show.optimum = TRUE) 
g = g + geom_text(data = as.data.frame(res$opt.path), mapping = aes(label = dob), color = "white")
g = g + geom_line(data = as.data.frame(res$opt.path), mapping = aes(x = x1, y = final.x.x2))
g

#FIXME: Make working: ?
#plot(res$opt.state)

### Parameter will be also learned

ctrl = makeMBOControl(final.method = "best.predicted")
ctrl = setMBOControlConceptDrift(
  control = ctrl,
  drift.function = slow.drift,
  learn.drift = TRUE,
  calculate.th.final.point = TRUE)
ctrl = setMBOControlTermination(ctrl, iter = mbo.iters)

res = mbo(fun = w.fn, control = ctrl)

res$opt.path$window.function = identity
as.data.frame(res$opt.path)
g = autoplot(fn, render.levels = TRUE, show.optimum = TRUE) 
g = g + geom_text(data = as.data.frame(res$opt.path), mapping = aes(label = dob), color = "white")
g = g + geom_line(data = as.data.frame(res$opt.path), mapping = aes(x = x1, y = final.x.x2))
g

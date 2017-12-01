mydev()
library(ggplot2)
fn = makeAlpine02Function(2)
#fn = makeBraninFunction()
autoplot(fn, render.levels = TRUE)

# we will take x1 as a time factor and optimize over x2

# questions
# For the intial design, the concept does not change?
w.fn = wrapSmoofConceptDrift(fn = fn, drift.param = "x1")

mbo.iters = 30
start.drift = 0.2
end.drift = -2
drift.range = unlist(attr(w.fn, "original.par.set")$pars[[attr(w.fn, "drift.param")]][c("lower", "upper")])
drift.range = drift.range + c(start.drift, end.drift)

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

plot(res$final.opt.state)

res$opt.path$window.function = identity
as.data.frame(res$opt.path)
g = autoplot(fn, render.levels = TRUE, show.optimum = TRUE)
g = g + geom_text(data = as.data.frame(res$opt.path), mapping = aes(label = dob), color = "white")
g = g + geom_line(data = as.data.frame(res$opt.path), mapping = aes(x = x1, y = final.x.x2))
g

#FIXME: Make working: ?
#plot(res$final.opt.state)

### Parameter will be also learned

ctrl = makeMBOControl(final.method = "best.predicted")
ctrl = setMBOControlConceptDrift(
  control = ctrl,
  drift.function = slow.drift,
  window.function = tail.window,
  learn.drift = TRUE,
  calculate.th.final.point = TRUE)
ctrl = setMBOControlTermination(ctrl, iter = mbo.iters)

res = mbo(fun = w.fn, control = ctrl)

plot(res$final.opt.state)
ggsave("tmp.pdf")

res$opt.path$window.function = identity
as.data.frame(res$opt.path)
g = autoplot(fn, render.levels = TRUE, show.optimum = TRUE)
g = g + geom_text(data = as.data.frame(res$opt.path), mapping = aes(label = dob), color = "white")
g = g + geom_line(data = as.data.frame(res$opt.path), mapping = aes(x = x1, y = final.x.x2))
g

### a hard cut

fun = makeSwiler2014Function()
fun(x = list(x1 = "1", x2 = 0.2, x3 = 0.2))
fun = wrapSmoofConceptDriftDiscrete(fun, "x1")
fun(x = list(x1 = 1, x2 = 0.2, x3 = 0.2))
plot2DNumeric(fun)

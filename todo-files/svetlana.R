library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(mlr)
library(soobench)
library(mco)

load_all(".", reset=TRUE)

set.seed(6)

z =load2("~/Desktop/saveopt.RData")
print(plotOptPath(z$opt.path, z$control))
ps = z$par.set
opdf = as.data.frame(z$opt.path)
xy = as.data.frame(z$opt.path, include.rest = FALSE)
task = makeRegrTask(data = xy, target = "y")

z$learner$fix.factors = FALSE
mod = train(z$learner, task)
des = generateRandomDesign(1000L, z$opt.path$par.set)
pred = predict(mod, newdata = des)$data$response
idx = pred < 0.43
good = des[idx, ]
r= sapply(good, range)
print(r)



library(soobench)

load_all(".")

set.seed(1)

obj = generate_bbob2009_function(2L, fid=17L, iid=1)

ctrl = makeMBOControl(
  noisy = FALSE,
  init.design.points = 10L,
  iters = 5L,
  propose.points = 5L, 
  multipoint.method = "moimbo",
  multipoint.moimbo.objective = "mean.se.dist",
  multipoint.moimbo.eta = 15,
  multipoint.moimbo.p = 1,
  multipoint.moimbo.maxit = 200L,
  infill.crit="ei"
)

lrn = makeLearner("regr.km", covtype="matern3_2", predict.type="se")
 # run = exampleRun(obj, learner=lrn, control=ctrl)
print(
  autoplot(run, pause=T, trafo=list(
      y=logTrafo(), 
      yhat=logTrafo(),
      se=logTrafo()
  ))$pl.all
)

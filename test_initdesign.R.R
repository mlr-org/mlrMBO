library(mlrMBO)
library(soobench)

objfun=generate_branin_function()

ps = makeNumericParamSet(len = number_of_parameters(objfun), lower = lower_bounds(objfun), upper = upper_bounds(objfun))

design.x=generateDesign(30, ps)
y=apply(design.x,1,objfun)
design=cbind(design.x,y)
attr(design, "trafo")=FALSE
learner_km=makeLearner("regr.km", predict.type="se", covtype="matern3_2",nugget.estim=TRUE)

ctrl = makeMBOControl(
iters = 50,
infill.crit="ei",
init.design.points=10,
infill.opt="focussearch")

m=mbo(makeMBOFunction(objfun), design=design, par.set=ps, learner=learner_km, control=ctrl, show.info=TRUE)
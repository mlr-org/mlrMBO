rm(list=ls())

### mein Zusatz
l=list.files(path = "C:/Users/bauer/Desktop/PROBE_mlr/mlrMBO_2/R")
setwd("C:/Users/bauer/Desktop/PROBE_mlr/mlrMBO_2/R")
for (i in 1:length(l)) source(l[i])

#source("C:/Users/bauer/Dropbox/NADJA/R-Analyse/LION2014/ParamHelpers/R/getBounds.R")
#getUpper2=getUpper

library(mlrMBO)
#configureMlr(on.learner.error = "warn",on.par.without.desc ="warn")



## Fitness function
library(soobench)
objfun1=generate_branin_function()


objfun2=function(listOfValues)
{
  x=listOfValues[[1]]
  k=listOfValues[[2]]
  method=listOfValues[[3]]
  perf= ifelse(listOfValues[[3]]=="a", k*sin(x)+cos(x),
               sin(x)+k*cos(x))
  return(perf)
}

#objfun2(list(1,2,"a"),4)


## Parameters
library(ParamHelpers)
#source("C:/Users/bauer/Dropbox/NADJA/R-Analyse/LION2014/ParamHelpers/R/getBounds.R")
library(testthat)
library(BBmisc)

par.set1 = makeNumericParamSet(len = number_of_parameters(objfun1), lower = lower_bounds(objfun1), upper = upper_bounds(objfun1))
par.set2=makeParamSet(
  makeNumericParam("x", lower=0,upper=1),
  makeIntegerParam("k", lower=1, upper=2),
  makeDiscreteParam("method", values=c("a", "b"))
)



# Initial design

library(lhs)
init.design.points1=5*sum(getParamLengths(par.set1))
init.design.fun1=randomLHS
set.seed(1)
design1=generateDesign(n=init.design.points1, par.set=par.set1, fun=init.design.fun1, trafo=FALSE)

# will be used later as makeMBOControl()  arguments
init.design.points2=5*sum(getParamLengths(par.set2))
init.design.fun2=maximinLHS
init.design.args2=list(k=3,dup=4)



# Surrogat Model
library(mlr)
learner_km=makeLearner("regr.km", predict.type="se", covtype="matern3_2")
learner_rf=makeLearner("regr.randomForest")



# Control
library(BBmisc)
control1 = makeMBOControl(
  iters = 2, 
  infill.crit="ei",
  infill.opt.fun="focus.search")

#FIXME Hyperparameter für maximinlhs übergeben!
control2 = makeMBOControl(
  minimize=FALSE,
  iters = 2, 
  infill.crit="mean",
  infill.opt.fun="ea",
  init.design.points=init.design.points2,
  init.design.fun=init.design.fun2)




# Experiments

# As the first argument of the objective function has to be a list of values and for the
# objfun1 it is two-dementional numeric vector, we wrapp objfun1 with makeMBOFunction() function
# which was created extra for this purpose

mbo1=mbo(makeMBOFunction(objfun1), par.set1, design=design1, learner=learner_km, control=control1, show.info=TRUE)


control1$infill.opt.fun="ea"
mbo1=mbo(makeMBOFunction(objfun1), par.set1, design=design1, learner=learner_km, control=control1, show.info=TRUE)

control1$infill.opt.fun="cmaes"
mbo1=mbo(makeMBOFunction(objfun1), par.set1, design=design1, learner=learner_km, control=control1, show.info=TRUE)

control1$infill.crit="mean"
mbo1=mbo(makeMBOFunction(objfun1), par.set1, design=design1, learner=learner_km, control=control1, show.info=TRUE)



# mbo 2

#ok
mbo2=mbo(objfun2, par.set2, design=NULL, learner=learner_rf, control=control2, show.info=TRUE)

#
control2$infill.crit="ei"
mbo2=mbo(objfun2, par.set2, design=NULL, learner=learner_rf, control=control2, show.info=TRUE)
#Error in checkStuff(fun, par.set, design, learner, control) : 
#  For infill criterion 'ei' predict.type of learner regr.randomForest must be set to 'se'!

# => 
listLearners(type="regr",se=TRUE)
#"regr.km"           "regr.lm"           "regr.randomForest"

# slowly but ok
learner_rf=makeLearner("regr.randomForest",predict.type="se")
mbo2=mbo(objfun2, par.set2, design=NULL, learner=learner_rf, control=control2, show.info=TRUE)

# what about other learners? like rpart? Do bagging!
learner_rp=makeLearner("regr.rpart")
bag_rp=makeBaggingWrapper(learner_rp,bag.iters=4,predict.type="se")
mbo2=mbo(objfun2, par.set2, design=NULL, learner=bag_rp, control=control2, show.info=TRUE)
#ok







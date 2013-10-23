library(methods)
library(testthat)
library(devtools)
library(mlr)
library(emoa)
library(mco)

load_all("skel", reset=TRUE)

makeAugmentedTschebycheff = function(design, y.ids) {
  ydim = length(y.ids)
  rho = 0.05
  lambda = runif(ydim, min=0, max=1) 
  lambda = lambda / sum(lambda)
  cns = c("y1", "y2")
  Y = design[,cns]
  new.target = apply(Y, 1, function(y) {
    a = lambda * y 
    max(a) + rho * sum(a)
  })
  X = design[, setdiff(colnames(design), y.ids)]
  cbind(X, y=new.target)
}


parego = function(fun, par.set, design=NULL, learner, control, show.info=TRUE, ...) {

  # get parameter ids repeated length-times and appended number
  rep.pids = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  y.name = control$y.name
  y.ids = c("y1", "y2")
  opt.path = makeOptPathDF(par.set, y.ids, minimize=c(TRUE, TRUE))

  X = design[, setdiff(colnames(design), y.ids)]
  Y = design[, y.ids]
	for (i in 1:nrow(design)) {
    addOptPathEl(opt.path, x=list(x=as.numeric(as.list(X[i,]))), y=as.numeric(Y[i,]), dob=0)
	}
  
	# do the mbo magic
  for (loop in seq_len(control$iters)) {
    print(loop)
    
    newdes = makeAugmentedTschebycheff(design, y.ids)
    rt = makeRegrTask(data=newdes, target="y")
    model = train(learner, rt)
    
		# propose new points and evaluate target function
    prop.design = proposePoints(model, par.set, control, newdes)
    xs = lapply(seq_len(nrow(prop.design)), function(i) dfRowToList(prop.design, par.set, i))
    ys = lapply(xs, fun)
    addOptPathEl(opt.path, x=xs[[1]], y=ys[[1]], dob=loop)
    design = rbind(design, cbind(prop.design, setColNames(as.data.frame(as.list(ys[[1]])), y.ids)))
  }

  #design = getTaskData(rt, target.extra=TRUE)$data
  #final.index = chooseFinalPoint(fun, par.set, model, opt.path, y.name, control)
  #best = getOptPathEl(opt.path, final.index)
  #x = best$x
  #y = best$y
  return(as.data.frame(opt.path))
}

objfun1 = function(x) zdt1(x) / c(1, 6)
objfun2 = function(x) {
  objfun1(x$x)
}
ps = makeNumericParamSet(len=3, lower=0, upper=1)
des = generateDesign(10, ps)
y = setColNames(t(apply(des, 1, objfun1)), c("y1", "y2"))
des = cbind(des, y)
lrn = makeLearner("regr.km", covtype="matern3_2", predict.type="se")
ctrl = makeMBOControl(iters=60, propose.points=1, infill.crit="ei", 
  infill.opt="random", infill.opt.random.points=1000)

op = parego(objfun2, ps, des, lrn, ctrl)
n = nrow(op)
cols = topo.colors(n)[op$dob]
plot(op$y1, op$y2, col=cols, pch=19)

ss = seq(0, 1, length.out=30)
front = expand.grid(x1=ss, x2=ss, x3=ss)
front = t(apply(front, 1, objfun1))
ranks = nds_rank(t(front))
front = front[ranks == 1,]
points(front[,1], front[,2], pch=19, col="red")


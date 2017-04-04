devtools::load_all()
options(error = recover)

obj.fun = makeSingleObjectiveFunction(
  name = "Mixed functions",
  fn = function(x) {
    if (x$cat == "a")
      x$num^2
    else
      x$num^2 + 3
  },
  par.set = makeParamSet(
    makeDiscreteParam("cat", values = c("a", "b")),
    makeNumericParam("num", lower = -5, upper = 5)
  ),
  has.simple.signature = FALSE,
  global.opt.value = -1
)

ctrl = makeMBOControl(propose.points = 1L)
ctrl = setMBOControlTermination(ctrl, iters = 10L)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(), opt = "focussearch", opt.focussearch.points = 500L)

rctrl = makeMBOControl()
rctrl = setMBOControlInfill(rctrl, crit = crit.cb1)
refinement.learner = makeLearner("regr.km", predict.type = "se", nugget.estim = TRUE)
ctrl = setMBOControlRefinement(ctrl, refinement.control = rctrl, refinement.learner = refinement.learner)

lrn = makeMBOLearner(ctrl, obj.fun)
design = generateDesign(4L, getParamSet(obj.fun), fun = lhs::maximinLHS)
run = mbo(obj.fun, design = design, learner = lrn, control = ctrl, show.info = TRUE)




if (FALSE) {
  options(error = NULL)

  refinement.learner = makeLearner("regr.randomForest", predict.type = "se")
  lrn = refinement.learner
  task = makeRegrTask(data = iris, target = "Petal.Length")
  learner = lrn = makeRefinementWrapper(lrn, factor.values = list(Species = factor("setosa")))
  data = getTaskData(task)
  args = list(Species = factor("setosa"))
  target = "Petal.Length"
  ARGS = NULL
  crossval(lrn, task)
}

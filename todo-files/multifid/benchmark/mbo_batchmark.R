batchmark = function(reg, learners, tasks, resamplings, measures = NULL, repls = 1L, save.opt.result = FALSE, overwrite = FALSE, pm.opts = list()) {
  # not needed for BE>=1.4, remove then
  fixID = function(x) gsub(".", "_", x, fixed = TRUE)

  BatchExperiments:::checkExperimentRegistry(reg)
  if ("mlr" %nin% names(reg$packages))
    stop("mlr is required on the slaves, please add mlr via 'addRegistryPackages'")

  learners = ensureVector(learners, 1L, cl = "Learner")
  assertList(learners, types = "Learner", min.len = 1L)
  learner.ids = vcapply(learners, "[[", "id")
  if (anyDuplicated(learner.ids))
    stop("Duplicated learner ids found")

  tasks = ensureVector(tasks, 1L, cl = "Task")
  assertList(tasks, types = "Task", min.len = 1L)
  task.ids = vcapply(tasks, getTaskId)
  if (anyDuplicated(task.ids))
    stop("Duplicated task ids found")

  resamplings = ensureVector(resamplings, length(tasks), "ResampleDesc")
  assertList(resamplings, "ResampleDesc", len = length(tasks))

  if (is.null(measures)) {
    measures = default.measures(tasks[[1L]])
  } else {
    measures = ensureVector(measures, 1L, "Measure")
    assertList(measures, types = "Measure", min.len = 1L)
  }

  assertCount(repls)
  assertFlag(save.opt.result)
  assertFlag(overwrite)
  assertList(pm.opts, names = "named")

  # generate problems
  pdes = Map(function(id, task, rdesc, seed) {
    static = list(rdesc = rdesc, task = task)
    addProblem(reg, id, static = static, dynamic = resample.fun, overwrite = overwrite, seed = seed)
    makeDesign(id, design = data.frame(i = seq_len(rdesc$iters)))
  }, id = fixID(task.ids), task = tasks, rdesc = resamplings, seed = reg$seed + seq_along(tasks))

  # generate algos
  ades = Map(function(id, learner) {
    apply.fun = getAlgoFun(learner, measures, save.opt.result, pm.opts)
    addAlgorithm(reg, id, apply.fun, overwrite = overwrite)
    makeDesign(id)
  }, id = fixID(learner.ids), learner = learners)

  # add experiments
  addExperiments(reg, prob.designs = pdes, algo.designs = ades, repls = repls, skip.defined = overwrite)
}

resample.fun = function(job, static, i) {
  rin = makeResampleInstance(desc = static$rdesc, task = static$task)
  list(train = rin$train.inds[[i]], test = rin$test.inds[[i]])
}

getAlgoFun = function(lrn, measures, save.opt.result, pm.opts) {
  force(lrn)
  force(measures)
  force(save.opt.result)
  force(pm.opts)
  function(job, static, dynamic) {
    if (length(pm.opts) > 0L) {
      do.call(parallelStart, pm.opts)
      on.exit(parallelStop())
    }
    model = train(learner = lrn, task = static$task, subset = dynamic$train)
    pred = predict(model, task = static$task, subset = dynamic$test)
    perf = performance(pred, measures, task = task, model = model)
    if (save.opt.result) {
      c(list(opt.result = model$learner.model$opt.result, 
             task.id = getTaskId(static$task)), learner.id = model$learner$id, performance = list(perf))
    } else {
      as.list(perf)
    } 
  }
}

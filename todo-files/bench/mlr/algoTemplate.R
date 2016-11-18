algoTemplate = function(static, iter, tctrl) {

  task = static$task
  tdesc = task$task.desc
  rdesc = makeResampleDesc("CV", iters = 2L)
  tw = makeTuneWrapper(mplexer, resampling = rdesc, par.set = par.set, control = tctrl)
  trainPredict(tw, static$task, static$rin, iter)
}


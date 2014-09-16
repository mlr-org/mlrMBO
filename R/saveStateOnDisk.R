# Check if current state should be saved on diks and do it if necessary.
# loop ist the current mbo-loop
saveStateOnDisk = function(loop, fun, learner, par.set, opt.path, control, show.info, more.args,
  models, resample.results, mbo.result) {
  # if required save on disk
  if (loop %in% control$save.on.disk.at) {
    save2(file = control$save.file.path,
      fun = fun,
      learner = learner,
      par.set = par.set,
      opt.path = opt.path,
      control = control,
      show.info = show.info,
      more.args = more.args,
      models = models,
      resample.results = resample.results,
      mbo.result = mbo.result,
      random.seed = .Random.seed
    )
    # and show some info
    if (loop <= control$iters)
      showInfo(show.info, "Saved the current state after iteration %i in the file %s.",
        loop, control$save.file.path)
    else
      showInfo(show.info, "Saved the final state in the file %s", control$save.file.path)
  }
}

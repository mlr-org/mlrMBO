saveStateOnDisk = function(loop, fun, learner, par.set, opt.path, control, show.info, more.args) {
  if (loop %in% control$save.on.disk.at) {
    save2(file = control$save.file.path,
      fun = fun,
      learner = learner,
      par.set = par.set,
      opt.path = opt.path,
      control = control,
      show.info = show.info,
      more.args = more.args
    )
  }
}

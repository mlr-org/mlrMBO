saveStateOnDisk = function(loop, control, ...) {
  if (loop %in% control$save.on.disk.at) {
    save2(list = as.list(...), file = control$save.file.path)
  }
}

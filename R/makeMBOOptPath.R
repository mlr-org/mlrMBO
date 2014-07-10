# Create the opt.path with MBO-specific defaults - 
# allways include error.message, exec.time and extra params, but never add transformed x.
makeMBOOptPath = function(par.set, control) {
  makeOptPathDF(
    par.set = par.set, 
    y.names = control$y.name,
    minimize = control$minimize,
    add.transformed.x = FALSE,
    include.error.message = TRUE, 
    include.exec.time = TRUE, 
    include.extra = TRUE
  )
}


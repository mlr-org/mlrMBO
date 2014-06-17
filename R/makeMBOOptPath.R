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


makeMBOOptPath = function(par.set, control) {
  makeOptPathDF(
    par.set = par.set, 
    y.names = control$y.name,
    minimize = control$minimize,
    include.error.message = TRUE, 
    include.exec.time = TRUE, 
    include.extra = TRUE
  )
}


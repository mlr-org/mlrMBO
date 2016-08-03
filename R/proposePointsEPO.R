# 

proposePointsEPO = function(opt.state) {
  
  opt.problem = getOptStateOptProblem(opt.state)
  models = getOptStateModels(opt.state)$models
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  iter = getOptStateLoop(opt.state)
  
  if (control$propose.points == 1L) {
    requirePackages("truncnorm")
    control2 = control
    control2$multicrit.epo.p = switch(control$multicrit.epo.p.method,
      const05 = 0.5,
      trunc.norm = rtruncnorm(n = 1, a = 0, b = 1, sd = 0.25,
        mean = (iter - 1) / control$iters),
      sqrt = sqrt((iter - 1) / control$iters), 
      linear = (iter - 1) / control$iters,
      runif = runif(1)
    )
    res = proposePointsByInfillOptimization(opt.state, control = control2)
  } else {
    # copy opt.path so we can store already proposed points in it
     opt.path2 = deepCopyOptPath(opt.path)
     dob = max(getOptPathDOB(opt.path)) + 1
     props = list()
     for (i in 1:control$propose.points) {
       
       control2 = control
       iter = getOptStateLoop(opt.state)
       
       requirePackages("truncnorm")
       control2$multicrit.epo.p = switch(control$multicrit.epo.p.method,
         const05 = 0.5,
         trunc.norm = rtruncnorm(n = 1, a = 0, b = 1, sd = 0.25,
           mean = (iter - 1) / control$iters),
         sqrt = sqrt((iter - 1) / control$iters), 
         linear = (iter - 1) / control$iters,
         runif = runif(1)
       )
       
       prop = proposePointsByInfillOptimization(opt.state, control = control2, opt.path = opt.path2)
       x = dfRowToList(prop$prop.points, par.set, 1)
       # y values not needed, NA is added
       addOptPathEl(opt.path2, x = x, y = rep(NA, control$n.objectives), dob = dob)
       props[[i]] = prop
     }
     res = joinProposedPoints(props)
  }
  return(res)
}


generateMBODesign.OptProblem = function(opt.problem) {
  control = getOptProblemControl(opt.problem)
  par.set = getOptStateParSet(opt.state)
  design.x = generateDesign(control$init.design.points, par.set, fun = control$init.design.fun, fun.args = control$init.design.args, trafo = FALSE)
  points.diff = control$init.design.points - nrow(design.x)
  if (points.diff > 0L) {
    warningf("Could not generate enough points for init design: Only got %i / %i. Augmenting with %i random points now!",
      nrow(design.x), control$init.design.points, points.diff)
    design.x.rand = generateRandomDesign(points.diff, par.set, trafo = FALSE)
    design.x = rbind(design.x, design.x.rand)
  }
  return(design.x)
}


# Implements a heuristic for proposed points. Points which are located too close to design
# points or already accepted poroposed points are dropped.
# 
# input:
#   prop [list]               : list of proposed points
#
# output:
#   prop.points [data.frame]  : modified proposed points

filterProposedPoints = function(prop, opt.path, par.set, control) {
  #FIXME: add something intelligent to handle discrete parameters
  if (hasDiscrete(par.set)) {
    warningf("Removing proposed points currently only realized for numeric only parameters.")
    return(prop)
  }

  # prepare stuff
  prop.points = prop$prop.points
  n = nrow(prop.points)
  tol = 0.2
  design = convertOptPathToDf(par.set, opt.path, control)
  design[[control$y.name]] = NULL

  euklideanDistance = function(x, y) {
    sqrt(sum((x - y)^2))
  }

  # storage for point indizes which should be removed
  to.delete = c()

  # check the proposed points
  for (i in 1:n) {
    prop.point = prop.points[i, ]
    min.distance.to.design.point = min(apply(design, 1, function(des.point) {
      euklideanDistance(prop.point, des.point)
    }))
    if (min.distance.to.design.point < tol) {
      to.delete = c(to.delete, i)
    } else {
      design = rbind(design, prop.point)
    }
  }
  
  # delete the points not accepted by our heuristic (too close to design points or already
  # accepted points) and all the other meta data linked to these points
  prop = lapply(prop, function(x) {
    if (is.matrix(x)) x[-to.delete, , drop = FALSE] else x[-to.delete]
  })

  return(prop)
}
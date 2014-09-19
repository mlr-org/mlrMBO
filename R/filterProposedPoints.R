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
  tol = control$filter.proposed.points.tol
  design = convertOptPathToDf(par.set, opt.path, control)
  design[[control$y.name]] = NULL

  euklideanDistance = function(x, y) {
    sqrt(sum((x - y)^2))
  }

  # storage for point indizes which should be removed
  to.delete = numeric(0)

  # check the proposed points
  for (i in 1:n) {
    prop.point = prop.points[i, ]
    # print(apply(design, 1, function(des.point) {
    #   euklideanDistance(prop.point, des.point)
    # }))
    min.distance.to.design.point = min(apply(design, 1, function(des.point) {
      euklideanDistance(prop.point, des.point)
    }))
    if (min.distance.to.design.point < tol) {
      catf("TOO close!")
      to.delete = c(to.delete, i)
    } else {
      design = rbind(design, prop.point)
    }
  }

  # for now replace removed design points with random points
  #FIXME: add function sampleValues for ParamSets to ParamHelpers
  n.replace = length(to.delete)

  if (n.replace > 0) {

    # sample points
    random.points = as.data.frame(lapply(par.set$pars, function(par) unlist(sampleValues(n = n.replace, par = par))))  
    random.points = list(prop.points = random.points, crit.vals = matrix(NA, nrow = n.replace), errors.model = rep(NA, n.replace))

    # delete the points not accepted by our heuristic (too close to design points or already
    # accepted points) and all the other meta data linked to these points
    prop = lapply(prop, function(x) {
      if (is.matrix(x) | is.data.frame(x)) x[-to.delete, , drop = FALSE] else x[-to.delete]
    })

    # maybe we have to handle this special property as well
    multipoint.lcb.lambdas = prop$multipoint.lcb.lambdas

    prop = joinProposedPoints(list(prop, random.points))
    if (!is.null(multipoint.lcb.lambdas)) {
      prop$multipoint.lcb.lambas = c(multipoint.lcb.lambdas, rep(NA, n.replace))
    }
  }

  return(prop)
}
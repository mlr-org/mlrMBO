# Generates a list of scalarized MBO multicrit tasks.
#
# @param design [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Initial design.
# @param y.name [\code{character(1)}]\cr
#   Name of y-column for target values in optimization path.
# @return [\code{\link[mlr]{SupervisedTask}]:
#   List of repaired points.
makeScalarTasks = function(design, par.set, y.name, control) {
  design$dob = design$eol = NULL
  if (any(sapply(design, is.integer)))
    design = as.data.frame(lapply(design, function(x) if(is.integer(x)) as.numeric(x) else x))
  if (any(sapply(design, is.logical)))
    design = as.data.frame(lapply(design, function(x) if(is.logical(x)) as.factor(x) else x))
  design = imputeFeatures(design, par.set, control)
  #if (control$rank.trafo)
  #  design[,y.name] = rank(design[,y.name])
  
  # normalize the targets to [0, 1]
  design.y = design[, control$y.name]
  design.y = sweep(design.y, 2, apply(design.y, 2, min))
  design.y = sweep(design.y, 2, apply(design.y, 2, max), "/")
  
  # Now Propose parEGO.propose.points points
  # Sample weighting vectors. Here we create twice the number we need and reject
  # the half with the smallest distance to another vector
  lambdas = matrix(nrow = 2 * control$parEGO.propose.points, ncol = control$number.of.targets)
  for(loop in 1:(2*control$parEGO.propose.points)) {
    # sample the lambda-vector
    repeat{
      lambda = sample(control$parEGO.s, control$number.of.targets, replace = TRUE)
      if (sum(lambda) == control$parEGO.s)
        break
    }
    lambdas[loop, ] = lambda
  }
  # Reject some lambdas ...
  while(nrow(lambdas) > control$parEGO.propose.points) {
    dists = as.matrix(dist(lambdas))
    dists[dists == 0] = Inf
    nearest = which.min(apply(dists, 1, min))
    lambdas = lambdas[-nearest, , drop= FALSE]
  }
  # Create the scalarized regression Tasks
  tasks = vector(mode = "list", length = control$parEGO.propose.points)
  for(loop in 1:control$parEGO.propose.points) {
    lambda = lambdas[loop, ]
    lambda = lambda / control$parEGO.s
    # Create the scalarized response 
    y.scalarized = sapply(1:nrow(design.y), function(i)
      max(lambda * design.y[i, ]) + control$parEGO.rho * sum(lambda * design.y[i, ]))
    regr.design = cbind(design, setColNames(data.frame(y.scalarized), "y.scalarized"))
    regr.design = regr.design[, -which(names(design) %in% control$y.name)]
    tasks[[loop]] = makeRegrTask(target = "y.scalarized", data = regr.design)
  }
  return(tasks)
}

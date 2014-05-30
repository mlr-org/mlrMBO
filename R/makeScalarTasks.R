# Generates a list of scalarized MBO multicrit tasks.
#
# @param design [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Initial design.
# @return [\code{\link[mlr]{SupervisedTask}]:
#   List of repaired points.
makeScalarTasks = function(design, par.set, control, Lambdas) {
  design$dob = design$eol = design$error.message = NULL
  if (any(sapply(design, is.integer)))
    design = as.data.frame(lapply(design, function(x) if(is.integer(x)) as.numeric(x) else x))
  if (any(sapply(design, is.logical)))
    design = as.data.frame(lapply(design, function(x) if(is.logical(x)) as.factor(x) else x))
  design = imputeFeatures(design, par.set, control)
  
  # normalize the targets to [0, 1]
  design.y = design[, control$y.name]
  design.y = sweep(design.y, 2, apply(design.y, 2, min))
  design.y = sweep(design.y, 2, apply(design.y, 2, max), "/")
  
  # Propose parEGO.propose.points points
  # If desired - create the margin weight vector
  margin.points = diag(control$number.of.targets)[control$parEGO.use.margin.points, , drop = FALSE]
  # How many random weights should be used?
  random.weights <- control$parEGO.propose.points - sum(control$parEGO.use.margin.points)
  if (random.weights > 0) {
    # Sample weighting vectors using a rejection method. Sample a discrete weight
    # vector and test if it sums up to 1. Here we create twice the number we need 
    # and reject the half with the smallest distance to another vector
    lambdas = matrix(nrow = control$parEGO.sample.more.weights * random.weights, ncol = control$number.of.targets)
    for (loop in 1:(control$parEGO.sample.more.weights * random.weights)) {
      # sample the lambda-vector
      repeat {
        lambda = Lambdas[sample(nrow(Lambdas), 1), ]
        # make sure every lambda is unique
        if (any(sapply(seq_len(nrow(lambdas)), function(i) all(lambdas[i, ] == lambda)), na.rm = TRUE))
          next
        if (any(sapply(seq_len(nrow(margin.points)), function(i) all(margin.points[i, ] == lambda)), na.rm = TRUE))
          next
        if (sum(lambda) == 1)
          break
      }
      lambdas[loop, ] = lambda
    }
    # Reject some lambdas ...
    while (nrow(lambdas) > random.weights) {
      dists = as.matrix(dist(lambdas))
      dists[dists == 0] = Inf
      nearest = which.min(apply(dists, 1, min))
      lambdas = lambdas[-nearest, , drop = FALSE]
    }
  } else {
    lambdas = matrix(nrow = 0, ncol = control$number.of.targets)
  }
  lambdas = rbind(margin.points, lambdas)
  
  # Create the scalarized regression Tasks
  tasks = vector(mode = "list", length = control$parEGO.propose.points)
  for (loop in 1:control$parEGO.propose.points) {
    lambda = lambdas[loop, ]
    # Create the scalarized response 
    y.scalarized = sapply(1:nrow(design.y), function(i)
      max(lambda * design.y[i, ]) + control$parEGO.rho * sum(lambda * design.y[i, ]))
    regr.design = cbind(design, setColNames(data.frame(y.scalarized), "y.scalarized"))
    regr.design = regr.design[, -which(names(design) %in% control$y.name)]
    tasks[[loop]] = makeRegrTask(target = "y.scalarized", data = regr.design)
  }
  return(list(tasks = tasks, weights = lambdas))
}

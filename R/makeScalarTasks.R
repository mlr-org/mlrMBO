# Generates a list of scalarized MBO multicrit tasks by sampling a unique weight
# vector for each task. 
#
# params: design, par.set and control as known. all.possible.weights is a
# matrix with control$number.of.targets cols, each row sums up to one. The weight
# vectors are sampled uniquely from this rows. Notice: The first control$number.of.target rows
# have to be the margin rows, as generated in mboParego
#
# @return list of
#   tasks: list of tasks
#   weights: matrix of used weight vectors
makeScalarTasks = function(design, par.set, control, all.possible.weights) {
  design$dob = design$eol = design$error.message = NULL
  design = convertDataFrameCols(design, ints.as.num = TRUE, logicals.as.factor = TRUE)
  # FIXME Use mlr here!
  design = imputeFeatures(design, par.set, control)
  
  # normalize the targets to [0, 1]
  design.y = design[, control$y.name]
  design.y = sweep(design.y, 2, apply(design.y, 2, min))
  design.y = sweep(design.y, 2, apply(design.y, 2, max), "/")
  
  # Propose parego.propose.points points
  # If desired - create the margin weight vector
  margin.points = diag(control$number.of.targets)[control$parego.use.margin.points, , drop = FALSE]
  
  # How many random weights should be used?
  random.weights = control$parego.propose.points - sum(control$parego.use.margin.points)
  # sample the lambda-vectors as rows from the matrix of all possible weights
  # if margin points should be used, exclude the corresponding rows
  # the margin rows are allways the first rows!
  # sample control$parego.sample.more.weights times the number of weights
  inds = setdiff(seq_row(all.possible.weights), which(control$parego.use.margin.points))
  lambdas = all.possible.weights[sample(inds, random.weights * control$parego.sample.more.weights), , drop = FALSE]
  # Reject weights as long as we have too many. reject the one with smallest dist
  while (nrow(lambdas) > random.weights) {
    dists = as.matrix(dist(lambdas))
    dists[dists == 0] = Inf
    nearest = which.min(apply(dists, 1, min))
    lambdas = lambdas[-nearest, , drop = FALSE]
  }
  lambdas = rbind(margin.points, lambdas)
  
  # Create the scalarized regression Tasks
  tasks = vector(mode = "list", length = control$parego.propose.points)
  for (loop in 1:control$parego.propose.points) {
    lambda = lambdas[loop, ]
    # Make sure we allway minimize!
    min.cor = ifelse(control$minimize, 1, -1)
    # Create the scalarized response 
    y.scalarized = sapply(1:nrow(design.y), function(i)
      max(lambda * design.y[i, ] * min.cor) +
        control$parego.rho * sum(lambda * design.y[i, ] * min.cor))
    regr.design = cbind(design, setColNames(data.frame(y.scalarized), "y.scalarized"))
    regr.design = regr.design[, -which(names(design) %in% control$y.name)]
    tasks[[loop]] = makeRegrTask(target = "y.scalarized", data = regr.design)
  }
  return(list(tasks = tasks, weights = lambdas))
}

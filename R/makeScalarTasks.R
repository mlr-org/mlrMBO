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
makeScalarTasks = function(par.set, opt.path, control, all.possible.weights) {

  npoints = control$propose.points
  # get data + normalize the targets to [0, 1] + drop them from data
  data = convertOptPathToDf(par.set, opt.path, control, impute = TRUE)
  data = dropNamed(data, control$y.name)
  y = getOptPathY(opt.path)
  y = normalize(y, method = "range", margin = 2L)

  # Propose points
  # If desired - create the margin weight vector
  margin.points = diag(control$number.of.targets)[control$parego.use.margin.points, , drop = FALSE]

  # How many random weights should be used?
  random.weights = npoints - sum(control$parego.use.margin.points)
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
  tasks = vector(mode = "list", length = npoints)
  for (loop in 1:npoints) {
    # make sure to minimize, then create scalarized response
    lambda = lambdas[loop, ] * ifelse(control$minimize, 1, -1)
    y2 = y %*% diag(lambda)
    data$y.scalar = apply(y2, 1, max) + control$parego.rho * rowSums(y2)
    tasks[[loop]] = makeRegrTask(target = "y.scalar", data = data)
  }
  return(list(tasks = tasks, weights = lambdas))
}

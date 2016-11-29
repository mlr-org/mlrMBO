# Generates a list of scalarized MBO multi-objective tasks by sampling a unique weight
# vector for each task.
#
# @param opt.path [\code{\link[ParamHelpers]{optPath}}]\cr
#   Optimization path.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @param all.possible.weights [\code{matrix}]\cr
# Matrix with control$n.objectives cols, each row sums up to one. The weight
# vectors are sampled uniquely from this rows. Notice: The first control$number.of.target rows
# have to be the margin rows, as generated in mboParego
# @return [\code{list}] List with elements
#   tasks: list of tasks
#   weights: matrix of used weight vectors
makeTasksParEGO = function(opt.path, control, all.possible.weights) {
  n.points = control$propose.points
  # get data + normalize the targets to [0, 1] + drop them from data
  data = convertOptPathToDf(opt.path, control)
  data = dropNamed(data, control$y.name)
  y = getOptPathY(opt.path)
  if (control$multiobj.parego.normalize == "standard") {
    y = normalize(y, method = "range", margin = 2L)
  } else {
    front = mco::paretoFilter(y)
    if (nrow(front) != 1) {
      y.max = apply(y, 2, max)
      y.min = apply(y, 2, min)
      front.max = apply(front, 2, max)
      ranges = (y.max - y.min) / (front.max - y.min)
      y = normalize(y, method = "range", margin = 2L)
      y = y * matrix(ranges, nrow = nrow(y), ncol = ncol(y), byrow = TRUE)
    }
    else {
      # FIXME What to do if the front consist only 1 point?
      y = normalize(y, method = "range", margin = 2L)
    }
  }

  # propose points
  # if desired - create the margin weight vector
  margin.points = diag(control$n.objectives)[control$multiobj.parego.use.margin.points, , drop = FALSE]

  # how many random weights should be used?
  random.weights = n.points - sum(control$multiobj.parego.use.margin.points)
  # if we used margin weights, we don't want to sample them!
  # be aware, that the margin weights are allways stored in the first rows.
  allowed.weight.inds = setdiff(seq_row(all.possible.weights), which(control$multiobj.parego.use.margin.points))
  # sample control$multiobj.parego.sample.more.weights times the number of weights
  nb.sample = random.weights * control$multiobj.parego.sample.more.weights
  # sample the lambda-vectors as rows from the matrix of all possible weights
  lambdas = all.possible.weights[sample(allowed.weight.inds, nb.sample), , drop = FALSE]
  # reject weights as long as we have too many. reject the ones with smallest dist
  while (nrow(lambdas) > random.weights) {
    dists = as.matrix(dist(lambdas))
    dists[dists == 0] = Inf
    nearest = which.min(apply(dists, 1, min))
    lambdas = lambdas[-nearest, , drop = FALSE]
  }
  lambdas = rbind(margin.points, lambdas)

  # create the scalarized regression tasks
  tasks = vector(mode = "list", length = n.points)
  for (loop in seq_len(n.points)) {
    # make sure to minimize, then create scalarized response
    lambda = lambdas[loop,, drop = TRUE] * ifelse(control$minimize, 1, -1)
    y2 = y %*% diag(lambda)
    data$y.scalar = apply(y2, 1, max) + control$multiobj.parego.rho * rowSums(y2)
    tasks[[loop]] = makeRegrTask(target = "y.scalar", data = data)
  }
  attr(tasks, "weight.mat") = lambdas
  return(tasks)
}

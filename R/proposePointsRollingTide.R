# Proposes k points for reevaluation for noisy optimization
proposePointsRollingTide = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  
  k = control$multicrit.rtmbmo.k
  design = getOptPathX(opt.path)
  design = convertDataFrameCols(design, ints.as.num = TRUE, logicals.as.factor = TRUE)
  
  # Get mean performances for each point - each point can have multiple evaluations
  unique.design = unique(convertRowsToList(design))
  Y.mean = lapply(unique.design, function(tmp) {
    reevals = sapply(seq_row(design), function(i) all(tmp == design[i, ]))
    Ys = getOptPathY(opt.path, drop = FALSE)[reevals, , drop = FALSE]
    list(perf = colMeans(Ys), n.evals = sum(reevals))
  })
  
  # Calculate the non-dominated sorting rank for each point
  Ys = extractSubList(Y.mean, "perf")
  nds.rank = nds_rank(Ys)
  n.evals = extractSubList(Y.mean, "n.evals")
  
  # Propose k points for reevaluation
  prop.inds = numeric(k)
  for (i in 1:k) {
    # Original Rolling Tide: Propose that non-dominated point
    # w ith the least amount of reevaluations
    if (control$multicrit.rtmbmo.method == "original") {
      inds.rank = nds.rank == 1
      min.evals = min(n.evals[inds.rank])
      candidates = which(inds.rank & n.evals == min.evals)
      if (length(candidates) > 1)
        candidates = sample(candidates, 1)
      prop.inds[i] = candidates
    }
    
    # Tschebbysheff: Minimize both nds.rank and n.evals by applying and minimizing
    # Augmenten Tschebbyshaff Norm.
    if (control$multicrit.rtmbmo.method == "tschebbysheff") {
      weight = runif(1)
      targets = cbind(
        normalize(nds.rank, method = "range"),
        normalize(n.evals, method = "range")
        )
      targets = targets %*% diag(c(weight, 1 - weight))
      target = apply(targets, 1, max) + 0.05 * rowSums(targets)
      prop.inds[i] = which.min(target)
    }
    
    ## Increase evaluation counter for this point
    n.evals[prop.inds[i]] = n.evals[prop.inds[i]] + 1
  }
  
  list(
    prop.points = design[prop.inds, ],
    crit.vals = matrix(rep.int(NA_real_, k), nrow = k, ncol = 1L),
    propose.time = rep.int(NA_real_, k),
    prop.type = rep("rolling_tide", k),
    errors.model = rep.int(NA_character_, k)
  )
}

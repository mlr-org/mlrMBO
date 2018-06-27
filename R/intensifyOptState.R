intensifyOptState = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  
  switch(control$noisy.method, 
      "incumbent" = intensifyIncumbent(opt.state),
      "ocba" = intensifyOCBA(opt.state)
    )
}

intensifyIncumbent = function(opt.state) {

  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  op = as.data.table(getOptStateOptPath(opt.state))
  par.names = colnames(op)[1:(which(colnames(op) == "y") - 1)] #FIXME: This sucks
  
  # get a summary of the design  
  ds = getOptPathSummary(opt.state, par.names)
  nds = nrow(ds)

  # incumbent: current best point w. r. t. mean over all function evaluations
  # the newest point cannot be the incumbent, it is always a challenger
  # DOES NOT WORK FOR MULTIPOINT PROPOSAL YET
  inc = which.min(ds[- nds, ]$y) 
  # incumbent is replicated once in each iteration
  replicatePoint(opt.state, x = ds[inc, ..par.names], type = "incumbent", reps = 1L)

  # determine a set of challengers
  if (control$noisy.incumbent.nchallengers == 0L) {
    cls = c(nds)
  } else {
    # determine set of p points to be challenged against incumbent
    # incumbent is excluded (cannot be challenged against itself)
    # and new point is always set as a challenger
    # points are drawn randomly without replacement with probability prop. to their function value
    cls = setdiff(seq_len(nds), c(inc, nds))
    p = min(control$noisy.incumbent.nchallengers, nds - 2)
    probs = exp(- ds[cls, ]$y) / sum(exp(- ds[cls, ]$y))
    cls = sample(cls, size = p, prob = probs, replace = FALSE)
    cls = c(cls, nds)
  }

  # start the race
  for (cl in cls) {

    r = 1L
    replicatePoint(opt.state, x = ds[cl, ..par.names], type = paste("challenger"), reps = r)
    ds = getOptPathSummary(opt.state, par.names)

    # proceed as long as challenger has less runs than incumbent and is better than incumbent
    while((ds[cl, "runs"] < ds[inc, "runs"]) && (ds[cl, "y"] < ds[inc, "y"])) {
      r = 2L * r
      replicatePoint(opt.state, x = ds[cl, ..par.names], type = paste("challenger"), reps = r)
      ds = getOptPathSummary(opt.state, par.names)
    }

  }
  return(opt.state)
}

replicatePoint = function(opt.state, x, type, reps = 1L) {

  # replicate rows according to the number of desired replicates
  xs = seq_len(nrow(x))
  xrep = x[rep(xs, reps), ]
   
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  prop = makeProposal(control, xrep, prop.type = rep(type, nrow(xrep)))
  evalProposedPoints.OptState(opt.state, prop)

  return(opt.state)
}

getOptPathSummary = function(opt.state, par.names) {
    op = as.data.table(getOptStateOptPath(opt.state))
    ds = op[, .(y = mean(y), ysd = sd(y), runs = .N), by = par.names]
    return(ds)
}


intensifyOCBA = function(opt.state) {
  
  # some intialization
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptProblemParSet(opt.problem)

  op = as.data.table(getOptStateOptPath(opt.state))
  par.names = colnames(op)[1:(which(colnames(op) == "y") - 1)] #FIXME: This sucks
  
  # minimum number of replicates at each point 
  minrep = max(control$noisy.ocba.initial, 2L)

  # calculate summary of the dsign 
  ds = getOptPathSummary(opt.state, par.names)
  nds = nrow(ds)

  # make sure that initially, each point is evaluated at least minrep times 
  xinit = rep(seq_len(nds), pmax(minrep - ds$runs, 0))
  opt.state = replicatePoint(opt.state, x = ds[xinit, ..par.names], type = paste("initeval"))

  ds = getOptPathSummary(opt.state, par.names)
  add = distributeOCBA(ds, budget = control$noisy.ocba.budget)
  reps = rep(seq_len(nds), add)
  
  replicatePoint(opt.state, x = ds[reps, ..par.names], type = paste("OCBA"))

  return(opt.state)
}


distributeOCBA = function(ds, budget) {

  nds = nrow(ds)

  # TODO: until now only minimization possible 
  tbudget = budget + sum(ds$runs)
  
  # search for the best and second-best dsign
  b = order(ds$y)[1]
  s = order(ds$y)[2]
  
  # vector of ratios
  ratio = rep(0, nds)
  ratio[s] = 1
  
  # calculate ratios
  tmp = (ds[b, ]$y - ds[s, ]$y) / (ds[b, ]$y - ds[- c(s, b), ]$y)
  ratio[- c(s, b)] = tmp^2 * ds[- c(s, b), ]$ysd^2 / ds[s, ]$ysd^2
  ratio[b] = ds[b, ]$ysd * sqrt(sum(ratio^2 / ds$ysd^2))
  
  # additional replications
  add = rep(0, nds)
  
  # do not disable any dsign
  disabled = rep(FALSE, nds)
  
  more_alloc = TRUE
  
  while (more_alloc) {
    
    add[!disabled] = roundPreserveSum(tbudget / sum(ratio[!disabled]) * ratio[!disabled])
    
    # disable designs that have been run too much
    disabled = disabled | (ds$runs > add)
    more_alloc = any(ds$runs > add)
    
    # set additional replications s.t. already run replications are set
    add[disabled] = ds[disabled, ]$runs
    
    # decrease total budget correspondingly
    tbudget = budget + sum(ds$runs) - sum(add[disabled])
  }
  
  add = add - ds$runs
  
  return(add)
}

roundPreserveSum = function(x, digits = 0) {
  up = 10 ^ digits
  x = x * up
  y = floor(x)
  indices = tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  y / up
}

identifyFinalPoints = function(opt.state, min.pcs = NULL, time.budget = NULL) {
  
  min.pcs = min.pcs %??% control$noisy.identification.pcs

  # some initialization
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptProblemParSet(opt.problem)

  op = as.data.table(getOptStateOptPath(opt.state))
  par.names = colnames(op)[1:(which(colnames(op) == "y") - 1)] #FIXME: This sucks
 
  # calculate summary of the design 
  ds = getOptPathSummary(opt.state, par.names)
  nds = nrow(ds)

  # make sure that initially, each point is evaluated at least minrep times 
  xinit = rep(seq_len(nds), pmax(2 - ds$runs, 0))
  opt.state = replicatePoint(opt.state, x = ds[xinit, ..par.names], type = paste("initeval"))
 
  setOptStateTimeUsedIdentification(opt.state)
  terminate = getOptStateTerminationIdentification(opt.state)

  while(!terminate$term) {
    prop = proposePoints(opt.state)
    evalProposedPoints.OptState(opt.state, prop)
    finalizeMboLoop(opt.state)
    intensifyOptState(opt.state)
    ds = getOptPathSummary(opt.state, par.names)
    pcs = calculatePCS(ds)
    while (pcs < min.pcs && !terminate$term) {
      opt.state = intensifyOCBA(opt.state)
      ds = getOptPathSummary(opt.state, par.names)
      pcs = calculatePCS(ds)
      setOptStateTimeUsedIdentification(opt.state)
      terminate = getOptStateTerminationIdentification(opt.state)
    }
  }
  return(opt.state)
}


calculatePCS = function(ds, i = NULL) {
  # calculate the (approximate) probability of correct selection
  # best observed design

  if (is.null(i)) {
  	  ds = ds[order(ds$y), ]
  } else {
  	  ds = ds[c(i, setdiff(1:nrow(ds), i)), ]
  }

  b = 1

  # mean anc covariance for vector (y1 - yb, y2 - yb, ..., yn - yb)
  vf = rep(1, nrow(ds) - 1) 
  trafo.mat = cbind(vf, diag(- vf)) # transformation matrix A

  m = ds[b, ]$y - ds[- b, ]$y

  # covariance for vector (y1 - yb, y2 - yb, ..., yn - yb)
  # is obtained by multiplication with matrix A = [(1 -1, 0), (1, 0, -1)]
  # rule is cov(A * Y) = A * cov(Y) * t(A)
  sigma = trafo.mat %*% diag(ds$ysd^2 / ds$runs) %*% t(trafo.mat)

  pcs = pmvnorm(upper = 0, mean = m, sigma = sigma)

  return(pcs[1])
}

identifyFinalPoints = function(opt.state, min.pcs = NULL, time.budget = NULL) {

  # some initialization
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptProblemParSet(opt.problem)
  op = as.data.table(getOptStateOptPath(opt.state))
  par.names = colnames(op)[1:(which(colnames(op) == "y") - 1)] #FIXME: This sucks
  min.pcs = min.pcs %??% getOptStatePCS(opt.state)

  # calculate summary of the design
  ds = getOptPathSummary(opt.state, par.names)
  nds = nrow(ds)

  # set start time for identification here

  # make sure that initially, each point is evaluated at least minrep times
  xinit = rep(seq_len(nds), pmax(2 - ds$runs, 0))
  opt.state = replicatePoint(opt.state, x = ds[xinit, ..par.names], type = paste("initeval"))

  setOptStateTimeUsedIdentification(opt.state)
  terminate = getOptStateTerminationIdentification(opt.state)

  pcs = calculatePCS(opt.state)
  while(pcs < min.pcs) {
      ds = getOptPathSummary(opt.state, par.names)
      add = distributeOCBA(ds, budget = 3)
      reps = rep(seq_len(nrow(ds)), add)
      replicatePoint(opt.state, x = ds[reps, ..par.names], type = paste("identification"))
      setOptStateTimeUsedIdentification(opt.state)
      terminate = getOptStateTerminationIdentification(opt.state)
      showInfo(getOptProblemShowInfo(opt.problem), "[mbo] identification: P(CS) %.3f / %.3f", pcs, min.pcs)
      pcs = calculatePCS(opt.state)

      if(terminate$term)
        break
  }

  return(opt.state)
}


calculatePCS = function(opt.state) {
  # calculate the (approximate) probability of correct selection
  # best observed design
  op = as.data.table(getOptStateOptPath(opt.state))
  par.names = colnames(op)[1:(which(colnames(op) == "y") - 1)] #FIXME: This sucks
  ds = getOptPathSummary(opt.state, par.names)

  ds = ds[order(ds$y), ]
  b = 1

  # mean anc covariance for vector (y1 - yb, y2 - yb, ..., yn - yb)
  vf = rep(1, nrow(ds) - 1)
  trafo.mat = cbind(vf, diag(- vf)) # transformation matrix A

  m = ds[b, ]$y - ds[- b, ]$y

  # covariance for vector (y1 - yb, y2 - yb, ..., yn - yb)
  # is obtained by multiplication with matrix A = [(1 -1, 0), (1, 0, -1)]
  # rule is cov(A * Y) = A * cov(Y) * t(A)
  sigma = trafo.mat %*% diag(ds$ysd^2 / ds$runs) %*% t(trafo.mat)
 #  sigma = diag(sqrt(ds$ysd[-b]^2 / ds$runs[-b]^2 + ds$ysd[b]^2 / ds$runs[b]^2))

  pcs = pmvnorm(upper = 0, mean = m, sigma = sigma)

  return(pcs[1])
}

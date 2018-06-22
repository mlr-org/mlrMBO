intensifyOptState = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)

  par.set = getOptProblemParSet(opt.problem)
  intensify(opt.state, par.set)

}



intensify = function(opt.state, par.set) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  par.set = getOptProblemParSet(opt.problem)

  op = as.data.table(getOptStateOptPath(opt.state))
  par.names = colnames(op)[1:(which(colnames(op) == "y") - 1)] #FIXME: This sucks
  ## Calculate summary of the design
  ds = op[, .(y = mean(y), .N), by = par.names]
  n = nrow(ds)

  ## Line 3: Evaluate incumbent
  inc = which.min(ds$y)
  xinc = ds[inc, ..par.names]
  prop = makeProposal(control, xinc, prop.type = "incumbent")
  evalProposedPoints.OptState(opt.state, prop)

  ## Line 4: determine a set of challengers
  p = pmin(5, n - 2)
  cs = 1:n
  cs = cs[-c(inc, n)]
  probs = ds[cs, ]$y + (- 1) * pmin(ds[inc, ]$y, 0)
  probs = 1 / probs
  cs = sample(cs, size = p, prob = probs, replace = FALSE)

  ## line 5: new point is also a challenger
  cs = c(cs, n)

  for (c in cs) {

    # line 8: evaluate x
    xc = ds[c, ..par.names]
    prop = makeProposal(control, xc, prop.type = "challenger")
    evalProposedPoints.OptState(opt.state, prop)

    op = as.data.table(getOptStateOptPath(opt.state))
    ds = op[, .(y = mean(y), .N), by = par.names]

    while((ds[c, ]$N < ds[inc, ]$N) && (ds[c, ]$y < ds[inc, ]$y)) {
      xc = ds[c, ..par.names]
      prop = makeProposal(control, xc, prop.type = "challenger")
      evalProposedPoints.OptState(opt.state, prop)
      op = as.data.table(getOptStateOptPath(opt.state))
      ds = op[, .(y = mean(y), .N), by = par.names]

      if ((ds[c, ]$N == ds[inc, ]$N)) {
        inc = c # replace incumbent
      }
    }
  }

  return(opt.state)
}


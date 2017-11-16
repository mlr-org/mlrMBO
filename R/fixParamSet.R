fixParamSet = function(par.set, fixed.par) {
  assertNamed(fixed.par)
  pars = par.set$pars[names(fixed.par)]
  pars = Map(function(par, val) insert(par, list(lower = val, upper = val)), pars, fixed.par)
  par.set$pars[names(fixed.par)] = setNames(pars, names(fixed.par))
  par.set
}
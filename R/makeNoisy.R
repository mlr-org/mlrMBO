makeNoisy = function(fun, noise) {

  assertClass(fun, "smoof_function")
  assertClass(noise, "function")

  name = getName(fun)
  id = getID(fun)
  if(is.na(id))
    id = NULL
  description = getDescription(fun)
  vectorized = isVectorized(fun)
  par.set = getParamSet(fun)
  minimize = shouldBeMinimized(fun)
  tags = c(getTags(fun))
  global.opt = getGlobalOptimum(fun)
  global.opt.params = global.opt$param
  global.opt.value = global.opt$value
  local.opt = getLocalOptimum(fun)
  local.opt.params = local.opt$params
  local.opt.values = local.opt$values

  fn = function(x) fun(x) + rnorm(1, mean = 0, sd = noise(x))

  smoof.fn = makeSingleObjectiveFunction(name = name, id = id, description = description,
    fn = fn, vectorized = vectorized, par.set = par.set,
    noisy = TRUE, fn.mean = fun, minimize = minimize,
    tags = tags, global.opt.params = global.opt.params, global.opt.value = global.opt.value,
    local.opt.params = local.opt.params, local.opt.values = local.opt.values)

  class(smoof.fn) = c("smoof_noisy_single_objective_function", class(smoof.fn))

  return(smoof.fn)
}

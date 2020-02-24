# show info without if-statement
showInfo = function(show.info, ...) {
  if (show.info)
    messagef(...)
}


# load required extra packages
loadPackages = function(control) {
  if (control$infill.opt == "cmaes")
    requirePackages("cmaesr", why = "proposePoints")
  if (control$n.objectives == 1L && control$propose.points > 1L && control$multipoint.method == "moimbo")
    requirePackages("emoa", why = "proposePoints")
}


# for Parego: calculate all integer vectors of length k with sum n
combWithSum = function(n, k) {
  fun = function(n, k) {
    if (k == 1L)
      list(n)
    else
      unlist(lapply(0:n, function(i) Map(c, i, fun(n - i, k - 1L))),
        recursive = FALSE)
  }
  matrix(unlist(fun(n, k)), ncol = k, byrow = TRUE)
}

getFileBackupName = function(fn) {
  file.path(dirname(fn), sprintf(".~%s", basename(fn)))
}

getRandomSeed = function() {
  if (!exists(".Random.seed", .GlobalEnv))
    set.seed(NULL)
  get(".Random.seed", .GlobalEnv)
}

measureTime = function(expr, ee = parent.frame()) {
  before = proc.time()[3L]
  force(expr)
  as.numeric(proc.time()[3L] - before)
}

# checks if a parameter or par.set is only numeric and has no dependencies/requires
# FIXME: remove as soon as this is in ParamHelper
isSimpleNumeric = function(par) {
  isNumeric(par, include.int = TRUE) && !hasRequires(par)
}

getHyperParsString2 = function(learner, show.missing.values = TRUE) {
  hps = getHyperPars(learner)
  ns = names(hps)
  pars = getParamSet(learner)$pars[ns]
  s = mapply(paramValueToString, pars, hps, MoreArgs = list(show.missing.values = show.missing.values))
  paste(ns, s, sep = "=", collapse = ",")
}


getColorPalette = function() {
  # RColorBrewer::brewer.pal(11, "Spectral")
  c("#9E0142", "#D53D4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
}

getLeafLearner = function(learner) {
  if (inherits(learner, "BaseWrapper")) {
    return(getLeafLearner(learner$next.learner))
  }
  return(learner)
}

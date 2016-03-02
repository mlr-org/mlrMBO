# show info without if-statement
showInfo = function(show.info, ...) {
  if (show.info)
    messagef(...)
}


# load required extra packages
loadPackages = function(control) {
  if (control$infill.opt == "cmaes")
    requirePackages("cmaes", why = "proposePoints")
  if (control$n.objectives == 1L && control$propose.points > 1L && control$multipoint.method == "multicrit")
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

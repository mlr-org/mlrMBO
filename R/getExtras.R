# helper to get extras-list for opt.path logging

getExtras = function(n, prop, control, weight.mat = NULL) {
  # this happens in init design
  if (is.null(prop)) {
    k = ifelse(control$multicrit.method == "mspot", control$number.of.targets, 1L)
    prop = list(crit.vals = matrix(NA_real_, nrow = n, ncol = k), errors.model = NA_character_)
  }
  exs = vector("list", n)
  errs = prop$errors.model
  # if we only have one msg, replicate it
  if (length(errs) == 1L)
    errs = rep(errs, n)
  for (i in 1:n) {
    # if we use mspot, store all crit.vals
    if (control$multicrit.method == "mspot") {
      ex = as.list(prop$crit.vals[i, ])
      names(ex) = paste(control$infill.crit, control$y.name, sep = ".")
      ex$error.model = errs[i]
    } else {
      ex = list(prop$crit.vals[i, 1L], error.model = errs[i])
      names(ex)[1] = control$infill.crit
    }
    # if we use parallel LCB, store lambdas
    if (control$propose.points > 1L && control$multipoint.method == "lcb") {
      ex$multipoint.lcb.lambdas = attr(prop, "multipoint.lcb.lambdas")
    }
    # if we use parego, store weights
    if (control$number.of.targets > 1L && control$multicrit.method == "parego") {
      if (is.null(weight.mat))
        weight.mat = matrix(NA, nrow = n, ncol = control$number.of.targets)
      w = setNames(as.list(weight.mat[i, ]), paste0(".weight", 1:ncol(weight.mat)))
      ex = c(ex, w)
    }
    exs[[i]] = ex
  }
  return(exs)
}


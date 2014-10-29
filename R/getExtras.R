# helper to get extras-list for opt.path logging

getExtras = function(n, prop, train.time, control) {
  # this happens in init design
  if (is.null(prop)) {
    k = ifelse(control$number.of.targets > 1L && control$multicrit.method == "mspot", control$number.of.targets, 1L)
    prop = list(crit.vals = matrix(NA_real_, nrow = n, ncol = k), errors.model = NA_character_,
      filter.replace = rep(NA, n))
  }
  exs = vector("list", n)
  errs = prop$errors.model
  # if we only have one msg, replicate it
  if (length(errs) == 1L)
    errs = rep(errs, n)
  for (i in 1:n) {
    # if we use mspot, store all crit.vals
    if (control$number.of.targets > 1L && control$multicrit.method == "mspot") {
      ex = as.list(prop$crit.vals[i, ])
      names(ex) = paste(control$infill.crit, control$y.name, sep = ".")
      ex$error.model = errs[i]
    } else {
      ex = list(prop$crit.vals[i, 1L], error.model = errs[i])
      names(ex)[1] = control$infill.crit
    }
    # if we use singlecrit parallel LCB store lambdas
    if (control$propose.points > 1L &&
        (control$number.of.targets == 1L && control$multipoint.method == "lcb")) {

      lams = prop$multipoint.lcb.lambdas
      if (is.null(lams))
        lams = rep(NA_real_, n)
      ex$multipoint.lcb.lambda = lams[i]
    }
    # if we use parego, store weights
    if (control$number.of.targets > 1L && control$multicrit.method == "parego") {
      weight.mat = prop$weight.mat
      if (is.null(weight.mat))
        weight.mat = matrix(NA_real_, nrow = n, ncol = control$number.of.targets)
      w = setNames(as.list(weight.mat[i, ]), paste0(".weight", 1:ncol(weight.mat)))
      ex = c(ex, w)
    }
    # if we filtered proposed points, store flag
    if (control$filter.proposed.points) {
      ex$filter.replace = prop$filter.replace[i]
    }
    ex$train.time = if (i == 1) train.time else NA_real_
    exs[[i]] = ex
  }
  return(exs)
}


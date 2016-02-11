# helper to get extras-list for opt.path logging
#
# @param n: nr of points
# @param prop: result of proposePoints,
# @param train.time: scalar numeric, time it took to train the models
#
# returns a list of n sublists. each inner element looks like this:
# (k = nr.of.targets)
#
# <crit.vals>                                 criteria values for all proposed points
# train.time              numeric(1)          Time to train the model(s) that produced the points
#                                             only first slot of vector is used (if we have multiple points)
#                                             rest are NA
# propose.time            numeric(1)          time needed to propose the point
#                                             if we have indivual timings from the proposal mechanism,
#                                             we have one differrent value per point here.
#                                             if all were generated in one go, we only have 1 timing,
#                                             we store it in the slot for the first point, rest are NA
# errors.model            character(1)        possible err msgs, if point-producing model(s) crashed
#                                             replicated for all n points, if only one errmsg was passed
# filter.replace          logical(1)          if point filtering was enabled, we store whether the point
#                                             was replaced by a random one.
# multipoint.cb.lambda   numeric(1)          random lambda-value used in q-CB point proposal
#                                             one lambda for each point in that case
# .weight<j>              numeric(1)          weight vector sampled for multipoint ParEGO

getExtras = function(n, prop, train.time, control) {
  # this happens in init design
  if (is.null(prop)) {
    k = ifelse(control$number.of.targets > 1L && control$multicrit.method == "mspot", control$number.of.targets + 1, 1L)
    # pregenerate a dummmy "prop" data structure
    prop = list(crit.vals = matrix(NA_real_, nrow = n, ncol = k), propose.time = NA_real_, errors.model = NA_character_,
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
      names(ex) = c(paste(control$infill.crit, control$y.name, sep = "."), "hv.contr")
      ex$error.model = errs[i]
    } else {
      ex = list(prop$crit.vals[i, 1L], error.model = errs[i])
      names(ex)[1] = control$infill.crit
    }
    # if we use singlecrit parallel CB store lambdas
    if (control$propose.points > 1L &&
        (control$number.of.targets == 1L && control$multipoint.method == "cb")) {

      lams = prop$multipoint.cb.lambdas
      if (is.null(lams))
        lams = rep(NA_real_, n)
      ex$multipoint.cb.lambda = lams[i]
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
    ex$propose.time = NA_real_
    if (length(prop$propose.time) > 1L) {
      ex$propose.time = prop$propose.time[i]
    } else {
      ex$propose.time = if (i == 1) prop$propose.time else NA_real_
    }
    exs[[i]] = ex
  }
  return(exs)
}


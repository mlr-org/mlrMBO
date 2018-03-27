# helper to get extras-list for opt.path logging
#
# @param n: nr of points
# @param prop: result of proposePoints,
# @param train.time: scalar numeric, time it took to train the models
#
# returns a list of n sublists. each inner element looks like this:
# (k = nr.of.targets)
#
# <crit.vals>
# train.time              numeric(1)
# propose.time            numeric(1)
# errors.model            character(1)
# multipoint.cb.lambda    numeric(1)
# parego.weight.<j>         numeric(1)
#
# Please document the content in doc_mbo_OptPath.R

getExtras = function(n, prop, train.time, control) {
  # this happens in init design
  infill.crit = control$infill.crit
  infill.crit.id = getMBOInfillCritId(control$infill.crit)

  allow.crit.components = control$n.objectives == 1L && !is.null(getMBOInfillCritComponents(infill.crit)) && (is.null(control$multipoint.method) || control$multipoint.method != "moimbo")

  if (is.null(prop)) {
    k = ifelse(control$n.objectives > 1L && control$multiobj.method == "mspot", control$n.objectives + 1, 1L)
    # pregenerate a dummmy "prop" data structure
    prop = list(crit.vals = matrix(NA_real_, nrow = n, ncol = k), propose.time = NA_real_, errors.model = NA_character_, prop.type = rep("initdesign", n))
    # a) no infill crit components for MCO
    # b) infill crit is ignored for multi-point moimbo, in which case we don't use any components
    if (allow.crit.components) {
      prop$crit.components = getMBOInfillCritDummyComponents(infill.crit)
    }
    # if (control$multifid) {
    #   prop$crit.components = cbind.data.frame(prop$crit.components, getMBOInfillCritDummyComponents((makeMBOInfillCritMultiFid()))
    # }
  }
  exs = vector("list", n)
  errs = prop$errors.model
  # if we only have one msg, replicate it
  if (length(errs) == 1L)
    errs = rep(errs, n)
  for (i in seq_len(n)) {
    # if we use mspot, store all crit.vals
    if (control$n.objectives > 1L && control$multiobj.method == "mspot") {
      ex = as.list(prop$crit.vals[i, ])
      names(ex) = c(paste(infill.crit.id, control$y.name, sep = "."), "hv.contr")
      ex$error.model = errs[i]
    } else {
      ex = list(prop$crit.vals[i, 1L], error.model = errs[i])
      names(ex)[1] = infill.crit.id
    }
    # if we use singlecrit parallel CB store lambdas
    if (control$propose.points > 1L &&
        (control$n.objectives == 1L && control$multipoint.method == "cb")) {

      lams = prop$multipoint.cb.lambdas
      if (is.null(lams))
        lams = rep(NA_real_, n)
      ex$multipoint.cb.lambda = lams[i]
    }
    # if we use parego, store weights
    if (control$n.objectives > 1L && control$multiobj.method == "parego") {
      weight.mat = prop$weight.mat
      if (is.null(weight.mat))
        weight.mat = matrix(NA_real_, nrow = n, ncol = control$n.objectives)
      w = setNames(as.list(weight.mat[i, ]), paste0("parego.weight.", seq_col(weight.mat)))
      ex = c(ex, w)
    }
    ex$train.time = if (i == 1) train.time else NA_real_
    ex$prop.type = prop$prop.type[i]
    ex$propose.time = NA_real_
    if (length(prop$propose.time) > 1L) {
      ex$propose.time = prop$propose.time[i]
    } else {
      ex$propose.time = if (i == 1) prop$propose.time else NA_real_
    }
    # infill.crit components
    if (allow.crit.components) {
      ex = insert(ex, as.list(prop$crit.components[i,,drop = FALSE]))
    }
    exs[[i]] = ex
  }
  return(exs)
}

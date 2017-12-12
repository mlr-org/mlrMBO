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
# filter.replace          logical(1)
# multipoint.cb.lambdas   numeric(1)
# .weight<j>              numeric(1)
# predicted.time          numeric(1) - predicted running time
# predicted.time.se       numeric(1) - se of predicted running time
# scheduled.at            numeric(1) - time this job is believed to run after fist one of batch started
# scheduled.on            integer(1) - cpu this job is beliefed to run on
# scheduled.job           integer(1) - job number of generated jobs
# scheduled.priority      numeric(1) - proiority value
# ks.used                 logical(1) - true if job was scheduled via knapsack
# multipoint.cb.lambda    numeric(1)
# parego.weight.<j>       numeric(1)
#
# Please document the content in doc_mbo_OptPath.R
getExtras = function(n, prop, train.time, control) {
  # this happens in init design
  if (is.null(prop)) {
    k = ifelse(control$n.objectives > 1L && control$multicrit.method == "mspot", control$n.objectives + 1, 1L)
    # pregenerate a dummmy "prop" data structure
    prop = list(crit.vals = matrix(NA_real_, nrow = n, ncol = k), propose.time = NA_real_, errors.model = NA_character_,
      filter.replace = rep(NA, n), prop.type = rep("initdesign", n))
    ## make space for crit.components (not so fancy to do it here)
    if (control$n.objectives == 1L && control$infill.crit == "ei") {
      prop$crit.components = data.frame(se = NA_real_, mean = NA_real_)
    } else if (control$n.objectives == 1L && control$infill.crit == "cb") {
      prop$crit.components = data.frame(se = NA_real_, mean = NA_real_, lambda = NA_real_)
    } else if (control$n.objectives == 1L && control$infill.crit == "aei") {
      prop$crit.components = data.frame(se = NA_real_, mean = NA_real_, tau = NA_real_)  
    } else if (control$n.objectives == 1L && control$infill.crit == "eei") {
      prop$crit.components = data.frame(se = NA_real_, mean = NA_real_)  
    }
    if (control$multifid) {
      prop$crit.components = cbind.data.frame(prop$crit.components, mf.ei.last = NA_real_, mf.se = NA_real_, mf.alpha1 = NA_real_, mf.alpha2 = NA_real_, mf.alpha3 = NA_real_, mf.sd = NA_real_)
    }
    if (control$schedule.method == "asyn") {
      prop = c(prop, scheduled.on = NA_integer_, eval.state = NA_character_) 
    }
  }
  exs = vector("list", n)
  errs = prop$errors.model

  lams = prop$multipoint.cb.lambdas
  if (is.null(lams))
    lams = rep(NA_real_, n)
  
  weight.mat = prop$weight.mat
  if (is.null(weight.mat))
    weight.mat = matrix(NA_real_, nrow = n, ncol = control$n.objectives)

  predicted.time = prop$predicted.time
  predicted.time.se = prop$predicted.time.se
  if (is.null(predicted.time)) {
    predicted.time = rep(NA_real_, n)  
    predicted.time.se = rep(NA_real_, n)
  }
  if (is.null(predicted.time.se)) {
  	predicted.time.se = rep(NA_real_, n)
  }  

  # if we only have one msg, replicate it
  if (length(errs) == 1L)
    errs = rep(errs, n)

  for (i in 1:n) {
    # if we use mspot, store all crit.vals
    if (control$n.objectives > 1L && control$multicrit.method == "mspot") {
      ex = as.list(prop$crit.vals[i, ])
      names(ex) = c(paste(control$infill.crit, control$y.name, sep = "."), "hv.contr")
      ex$error.model = errs[i]
    } else {
      ex = list(prop$crit.vals[i, 1L], error.model = errs[i])
      names(ex)[1] = control$infill.crit
    }
	# if we use singlecrit parallel CB store lambdas
    if (!is.null(control$multipoint.method) && (control$n.objectives == 1L && control$multipoint.method == "cb")) {
      lams = prop$multipoint.cb.lambdas
      if (is.null(lams))
        lams = rep(NA_real_, n)
      ex$multipoint.cb.lambda = lams[i]
    }
    # if we use parego, store weights
    if (control$n.objectives > 1L && control$multicrit.method == "parego") {
      weight.mat = prop$weight.mat
      if (is.null(weight.mat))
        weight.mat = matrix(NA_real_, nrow = n, ncol = control$n.objectives)
      w = setNames(as.list(weight.mat[i, ]), paste0("parego.weight.", 1:ncol(weight.mat)))
      ex = c(ex, w)
    }
    # if we filtered proposed points, store flag
    if (control$filter.proposed.points) {
      ex$filter.replace = prop$filter.replace[i]
    }
    # if we use scheduling, store predicted exec.times
    if (control$schedule.method == "smartParallelMap"||control$schedule.method == "scheduleKnapsack") {
      ex$predicted.time = predicted.time[i]
      ex$predicted.time.se = predicted.time.se[i]
      ex$scheduled.at = NA_real_
      ex$scheduled.on = NA_integer_
      ex$scheduled.job = NA_real_
      ex$scheduled.priority = NA_real_
    } else if (control$schedule.method == "asyn") {
      ex$eval.state = prop$eval.state
      ex$scheduled.on = prop$scheduled.on
    }
    if(control$schedule.method == "scheduleKnapsack"){
      ex$ks.used = FALSE
    }
    if (isTRUE(attr(prop$prop.points, "constant.model"))) {
      ex$constant.model = TRUE
    } else {
      ex$constant.model = FALSE
    }
    # if we use asyn MBO store node information and evaluation starte
    ex$train.time = if (i == 1) train.time else NA_real_
    ex$prop.type = prop$prop.type[i]
    ex$propose.time = NA_real_
    if (length(prop$propose.time) > 1L) {
      ex$propose.time = prop$propose.time[i]
    } else {
      ex$propose.time = if (i == 1) prop$propose.time else NA_real_
    }
    ex$exec.timestamp = NA_integer_
    ex$pid = NA_integer_
    # infill.crit components
    ex = insert(ex, as.list(prop$crit.components[i,,drop = FALSE]))
    exs[[i]] = ex
  }
  return(exs)
}


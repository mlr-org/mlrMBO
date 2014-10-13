library(BatchExperiments)
library(parallelMap)
library(mlrMBO)
library(eaf)
library(stringr)
library(checkmate)


# copy-paste-crap, since this function is only nearly done. will be
# in ParamHelpers soon

# @title Plots attainment functions for data stored in multiple OptPaths.
#
# @description
# Can be used to plot OptPaths where information for bi-objective
# evaluation was logged for repeated runs of different algorithmic runs.
# Pretty directly calls \code{link[eaf]{eafplot}}.
#
# @template arg_parset
# @param [\code{list}]\cr
# List of list of \code{OptPath} objects. First index is the algorithm / major variation
# in the experiment, second index is the index of the replicated run.
# @param ... [any]\cr
# Passed on to \code{\link{eafplot}}.
# We changed the defaults in the following way:
# The axis are labeled by \code{y.names}.
# But this can again be overwritten by the user.
# @return [\code{data.frame}]. Invisibly returns the data passed to \code{\link{eafplot}}.
# @export
plotEAF = function(opt.paths, xlim = NULL, ylim = NULL, ...) {
  args = list(...)
  requirePackages("eaf", why = "plotEAF")
  assertList(opt.paths, min.len = 1L, types = "list", names = "unique")
  algos = names(opt.paths)
  y.names = NULL; minimize = NULL
  data = data.frame()
  for (i in seq_along(algos)) {
    a = algos[i]
    runs = opt.paths[[i]]
    assertList(runs, types = "OptPath", min.len = 1L)
    # combine all fronts for this algo + add algo / repl + do some sanity checks
    fronts = lapply(seq_along(runs), function(j) {
      run = runs[[j]]
      f = as.data.frame(getOptPathParetoFront(run))
      cns = colnames(f)
      if (length(cns) != 2L)
        stop("Must always have 2 objectives in opt path. But found: %i", length(cns))
      if (i == 1L && j == 1L) {
        y.names <<- cns
        minimize <<- run$minimize
      }
      if (!all(y.names == cns))
        stop("Must always have the same 2 objectives in opt path: %s (first ones taken). But found here: %s",
          collapse(y.names), collapse(cns))
      if (!all(minimize == run$minimize))
        stop("Must always have the same 'minimize' settings for objectives in opt path: %s (first one taken). But found here: %s",
          collapse(minimize), collapse(run$minimize))
      cbind(f, .algo = a, .repl = j)
    })
    fronts = do.call(rbind, fronts)
    data = rbind(data, fronts)
  }
  yn1 = y.names[1L]; yn2 = y.names[2L]
  f = as.formula(sprintf("%s + %s ~ .repl", yn1, yn2))
  defaults = list(
    xlab = yn1, ylab = yn2,
    percentiles = 50
  )
  args = insert(defaults, args)
  args$x = f
  args$data = data
  args$groups = quote(.algo)
  args$maximise = !minimize
  args$xlim = xlim
  args$ylim = ylim
  do.call(eafplot, args)
  return(data)
}

# Now get the results we want.
# We only consider 2M-function. An we only want our "best" algos
# dib-1-sms & parego-1-lcb & dib-4-eps & dib-4-sms & mspot-4-lcb & parego-4-lcb
reg = loadRegistry("mco_bench-files", work.dir = ".")
job.info = getJobInfo(reg, pars = TRUE)
job.info$algo2 = paste(job.info$algo, job.info$budget, job.info$prop.points,
  job.info$indicator, job.info$crit, sep = "-")
job.info$algo2 = str_replace_all(job.info$algo2, "-NA", "")
pids = c("GOMOP3_3D2M", "GOMOP_2D2M", "GOMOP_5D2M", "dtlz2_5D2M", "zdt1_5D2M", "zdt2_5D2M", "zdt3_5D2M")

aids.base = aids1 = c("nsga2-normal", "randomSearch-normal", "exactFront")
aids1 = c(aids.base, "dib-1-sms", "dib-1-eps")
aids2 = c(aids.base, "parego-1-ei", "parego-1-lcb")
aids3 = c(aids.base, "mspot-1-mean", "mspot-1-ei", "mspot-1-lcb")
aids4 = c(aids.base, "dib-4-sms","dib-4-eps")
aids5 = c(aids.base, "parego-4-ei", "parego-4-lcb")
aids6 = c(aids.base, "mspot-4-mean", "mspot-4-ei", "mspot-4-lcb")
aids7 = c(aids.base, "dib-1-sms", "parego-4-lcb", "mspot-4-ei")



getOptPaths = function(job.info, pids, aids) {
  ops = list()
  for (pid in pids) {
    for (aid in aids) {
      jids = job.info[job.info$prob == pid & job.info$algo2 == aid, "id"]
      assertInteger(jids, len = 20L, any.missing = FALSE)
      ops[[pid]][[aid]] = extractSubList(loadResults(reg, jids), "opt.path", simplify = FALSE)
    }
    names(ops[[pid]]) = paste(seq_along(aids), aids)
  }
  return(ops)
}
plotMMBOEAF = function(prob.id, opt.paths, file = NULL, logscale = FALSE,
  title = prob.id, xlim = NULL, ylim = NULL, ...) {
  # col = rainbow(length(aids))
  if (!logscale) {
    log = ""; legend.pos = "topright"
  } else {
    log = "xy"; legend.pos = "bottomleft"
  }
  if (!is.null(file)) pdf(file)
  d = plotEAF(opt.paths[[prob.id]], legend.pos = "topright", log = log, main = title,
    xlab = "y1", ylab = "y2", xlim = xlim, ylim = ylim, ...)
  if (!is.null(file)) dev.off()
  return(d)
}

xlims = list(c(-3.965, -3.6), c(0, 30), c(-3.5, 0), c(0, 1.25), c(0, 1), c(0, 1), c(0, 1))
ylims = list(c(1, 1e4),        c(0, 1),  c(30, 100), c(0, 1.25), c(0, 2), c(0, 2), c(-1, 4))
pdf("eafs.pdf")
for(j in 1:7) {
  pid = pids[j]
  for (i in 1:7) {
    aids = get(paste("aids", i, sep = ""))
    plotMMBOEAF(pid, getOptPaths(job.info, pid, aids), xlim = xlims[[j]], ylim = ylims[[j]])
  }
}
dev.off()

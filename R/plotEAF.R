#' @title Plots attainment functions for data stored in multiple OptPaths.
#'
#' @description
#' Can be used to plot OptPaths where information for bi-objective
#' evaluation was logged for repeated runs of different algorithmic runs.
#' Pretty directly calls \code{\link[eaf]{eafplot}}.
#'
#' @param opt.paths [\code{list}]\cr
#'   List of list of \code{OptPath} objects. First index is the algorithm / major variation
#'   in the experiment, second index is the index of the replicated run.
#' @param xlim [\code{numeric(2)}]\cr
#'   The x limits (x1, x2) of the plot.
#' @param ylim [\code{numeric(2)}]\cr
#'   The y limits (y1, y2) of the plot.
#' @param ... [any]\cr
#'   Passed on to \code{\link[eaf]{eafplot}}.
#' @note
#'   We changed the defaults of \code{\link[eaf]{eafplot}} in the following way:
#'   The axis are labeled by \code{y.names}. But this can again be overwritten by the user.
#' @return [\code{data.frame}]
#'   Invisibly returns the data passed to \code{\link[eaf]{eafplot}}.
#' @export
plotEAF = function(opt.paths, xlim = NULL, ylim = NULL, ...) {
  requirePackages("eaf", why = "plotEAF")

  # we need a list of lists with optimization pathes
  assertList(opt.paths, min.len = 1L, types = "list", names = "unique")
  algos = names(opt.paths)
  y.names = NULL
  minimize = NULL
  data = data.frame()
  for (i in seq_along(algos)) {
    # extract opt path list for current algorithm
    a = algos[i]
    runs = opt.paths[[i]]
    assertList(runs, types = "OptPath", min.len = 1L)

    # combine all fronts for this algo + add algo / repl + do some sanity checks
    fronts = lapply(seq_along(runs), function(j) {
      run = runs[[j]]
      f = as.data.frame(getOptPathParetoFront(run))
      cns = colnames(f)
      if (length(cns) != 2L) {
        stop("Must always have 2 objectives in opt path. But found: %i", length(cns))
      }
      if (i == 1L && j == 1L) {
        y.names <<- cns
        minimize <<- run$minimize
      }
      if (!all(y.names == cns)) {
        stop("Must always have the same 2 objectives in opt path: %s (first ones taken). But found here: %s",
          collapse(y.names), collapse(cns))
      }
      if (!all(minimize == run$minimize)) {
        stop("Must always have the same 'minimize' settings for objectives in opt path: %s (first one taken).
          But found here: %s", collapse(minimize), collapse(run$minimize))
      }
      # add column for algorithm and replication
      cbind(f, .algo = a, .repl = j)
    })
    fronts = do.call(rbind, fronts)
    data = rbind(data, fronts)
  }
  yn1 = y.names[1L]
  yn2 = y.names[2L]
  # build parameter list for eafplot function
  f = as.formula(sprintf("%s + %s ~ .repl", yn1, yn2))
  defaults = list(
    xlab = yn1, ylab = yn2,
    percentiles = 50
  )
  args = list(...)
  args = insert(defaults, args)
  args$x = f
  args$data = data
  args$groups = quote(.algo)
  args$maximise = !minimize
  args$xlim = xlim
  args$ylim = ylim
  args$col = c("darkgrey", "darkgrey", "darkgrey", "black", "black", "black")
  args$lty = c("solid", "dashed", "dotdash", "solid", "dashed", "dotdash")
  do.call(eaf::eafplot, args)
  return(data)
}

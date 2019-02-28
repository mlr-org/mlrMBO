#' @title Extends mbo control object with infill criteria and infill optimizer options.
#'
#' @description
#' Please note that internally all infill criteria are minimized. So for some of them,
#' we internally compute their negated version, e.g., for EI or also for CB when the objective is to
#' be maximized. In the latter case mlrMBO actually computes the negative upper confidence bound and
#' minimizes that.
#'
#' @template arg_control
#' @param crit [\code{\link{MBOInfillCrit}}]\cr
#'   How should infill points be rated. See \code{\link{infillcrits}} for an overview
#'   of available infill criteria or implement a custom one via \code{\link{makeMBOInfillCrit}}.#
#'   Default is \dQuote{(lower) confidence bound} (see \code{\link{makeMBOInfillCritCB}}).
#' @param interleave.random.points [\code{integer(1)}]\cr
#'   Add \code{interleave.random.points} uniformly sampled points additionally to the
#'   regular proposed points in each step.
#'   If \code{crit="random"} this value will be neglected.
#'   Default is 0.
#' @param filter.proposed.points [\code{logical(1)}]\cr
#'   Design points located too close to each other can lead to
#'   numerical problems when using e.g. kriging as a surrogate model.
#'   This may solve the 'leading minor of order ...' error during model fit.
#'   This parameter activates or deactivates a heuristic to handle this issue.
#'   If \code{TRUE}, proposed points whose distance to design points or other current
#'   candidate points is smaller than \code{filter.proposed.points.tol}, are replaced by random points.
#'   If enabled, the column entry for \code{prop.type} is set to \dQuote{random_filter} in the resulting \code{opt.path},
#'   so you can see whether such a replacement happened.
#'   This does only work for numeric parameter sets without any discrete parameters.
#'   Default is \code{FALSE}.
#' @param filter.proposed.points.tol [\code{numeric(1)}]\cr
#'   Tolerance value filtering of proposed points. We currently use a maximum metric
#'   to calculate the distance between points.
#'   Default is 0.0001.
#' @param opt [\code{character(1)}]\cr
#'   How should SINGLE points be proposed by using the surrogate model. Possible values are:
#'   \dQuote{focussearch}: In several iteration steps the parameter space is
#'   focused on an especial promising region according to infill criterion.
#'   \dQuote{cmaes}: Use CMA-ES (function \code{\link[cmaesr]{cmaes}} from package \pkg{cmaesr}
#'   to optimize infill criterion. If all CMA-ES runs fail, a random point is generated
#'   instead and a warning informs about it.
#'   \dQuote{ea}: Use an (mu+1) EA to optimize infill criterion.
#'   \dQuote{nsga2}: NSGA2 for multi obj. optimizations. Needed for mspot.
#'   Default is \dQuote{focussearch}.
#'   Alternatively, you may pass a function name as string.
#' @param opt.restarts [\code{integer(1)}]\cr
#'   Number of independent restarts for optimizer of infill criterion.
#'   If \code{opt == "cmaes"} the first start point for the optimizer is always the
#'   currently best point in the design of already visited points. Subsequent starting
#'   points are chosen according to the CMA-ES restart strategy introduced by Auger
#'   and Hansen. For details see the corresponding paper in the references and the help
#'   page of the underlying optimizer \code{\link[cmaesr]{cmaes}}.
#'   Default is 3.
#' @param opt.focussearch.maxit [\code{integer(1)}]\cr
#'   For \code{opt = "focussearch"}:
#'   Number of iteration to shrink local focus.
#'   Default is 5.
#' @param opt.focussearch.points [\code{integer(1)}]\cr
#'   For \code{opt = "focussearch"}:
#'   Number of points in each iteration of the focus search optimizer.
#'   Default is 1000.
#' @param opt.cmaes.control [\code{list}]\cr
#'   For \code{opt = "cmaes"}:
#'   Control argument for cmaes optimizer.
#'   For the default see the help page of the underlying optimizer \code{\link[cmaesr]{cmaes}}.
#' @param opt.ea.maxit [\code{integer(1)}]\cr
#'   For \code{opt = "ea"}:
#'   Number of iterations / generations of EA.
#'   Default is 500.
#' @param opt.ea.mu [\code{integer(1)}]\cr
#'   For \code{opt = "ea"}:
#'   Population size of EA.
#'   The default is 10 times the number of parameters of the function to optimize.
#' @param opt.ea.pm.eta [\code{numeric(1)}]\cr
#'   For \code{opt = "ea"}:
#'   Distance parameter of mutation distribution, see \code{\link[emoa]{pm_operator}}.
#'   Default is 15.
#' @param opt.ea.pm.p [\code{numeric(1)}]\cr
#'   For \code{opt = "ea"}:
#'   Probability of 1-point mutation, see \code{\link[emoa]{pm_operator}}.
#'   Default is 0.5.
#' @param opt.ea.sbx.eta [\code{numeric(1)}]\cr
#'   For \code{opt = "ea"}:
#'   Distance parameter of crossover distribution , see \code{\link[emoa]{sbx_operator}}.
#'   Default is 15.
#' @param opt.ea.sbx.p [\code{numeric(1)}]\cr
#'   For \code{opt = "ea"}:
#'   Probability of 1-point crossover, see \code{\link[emoa]{sbx_operator}}.
#'   Default is 0.5.
#' @param opt.ea.lambda [\code{numeric{1}}]\cr
#'   For \code{opt.ea = "ea"}.
#'   Number of children generated in each generation.
#'   Default is 1.
#' @param opt.nsga2.popsize [\code{numeric{1}}]\cr
#'   For \code{opt.multiobj.method = "nsga2"}.
#'   Population size of nsga2.
#'   Default is 100.
#' @param opt.nsga2.generations [\code{numeric{1}}]\cr
#'   For \code{opt.multiobj.method = "nsga2"}.
#'   Number of populations for of nsga2.
#'   Default is 50.
#' @param opt.nsga2.cprob [\code{numeric{1}}]\cr
#'   For \code{opt.multiobj.method = "nsga2"}.
#'   nsga2 param. Default is 0.7.
#' @param opt.nsga2.cdist [\code{numeric{1}}]\cr
#'   For \code{opt.multiobj.method = "nsga2"}.
#'   nsga2 param. Default is 5.
#' @param opt.nsga2.mprob [\code{numeric{1}}]\cr
#'   For \code{opt.multiobj.method = "nsga2"}.
#'   nsga2 param. Default is 0.2.
#' @param opt.nsga2.mdist [\code{numeric{1}}]\cr
#'   For \code{opt.multiobj.method = "nsga2"}.
#'   nsga2 param. Default is 10.
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlInfill = function(control,
  crit = NULL,
  interleave.random.points = 0L,
  filter.proposed.points = NULL,
  filter.proposed.points.tol = NULL,
  opt = "focussearch", opt.restarts = NULL,
  opt.focussearch.maxit = NULL, opt.focussearch.points = NULL,
  opt.cmaes.control = NULL,
  opt.ea.maxit = NULL, opt.ea.mu = NULL,
  opt.ea.sbx.eta = NULL, opt.ea.sbx.p = NULL,
  opt.ea.pm.eta = NULL, opt.ea.pm.p = NULL,
  opt.ea.lambda = NULL,
  #opt.multiobj.randomsearch.points = 50000L,
  opt.nsga2.popsize = NULL, opt.nsga2.generations = NULL,
  opt.nsga2.cprob = NULL, opt.nsga2.cdist = NULL,
  opt.nsga2.mprob = NULL, opt.nsga2.mdist = NULL) {

  assertClass(control, "MBOControl")

  control$infill.crit = crit %??% control$infill.crit %??% makeMBOInfillCritCB()
  assertClass(control$infill.crit, "MBOInfillCrit")

  assertCount(interleave.random.points)
  control$interleave.random.points = interleave.random.points

  control$filter.proposed.points = coalesce(filter.proposed.points, control$filter.proposed.points, FALSE)
  assertFlag(control$filter.proposed.points)

  control$filter.proposed.points.tol = coalesce(filter.proposed.points.tol, control$filter.proposed.points.tol, 1e-4)
  assertNumber(control$filter.proposed.points.tol, na.ok = FALSE, lower = 0)

  control$infill.opt = coalesce(opt, control$infill.opt, "focussearch")
  assertChoice(control$infill.opt, choices = getSupportedInfillOptFunctions())

  control$infill.opt.restarts = coalesce(opt.restarts, control$infill.opt.restarts, 3L)
  control$infill.opt.restarts = asCount(control$infill.opt.restarts)
  assertCount(control$infill.opt.restarts, na.ok = FALSE)

  control$infill.opt.focussearch.maxit = coalesce(opt.focussearch.maxit, control$infill.opt.focussearch.maxit, 5L)
  control$infill.opt.focussearch.maxit = asCount(control$infill.opt.focussearch.maxit)
  assertCount(control$infill.opt.focussearch.maxit, na.ok = FALSE, positive = TRUE)

  control$infill.opt.focussearch.points = coalesce(opt.focussearch.points, control$infill.opt.focussearch.points, 1000L)

  control$infill.opt.focussearch.points = asCount(control$infill.opt.focussearch.points)
  assertCount(control$infill.opt.focussearch.points, na.ok = FALSE, positive = TRUE)

  control$infill.opt.cmaes.control = coalesce(opt.cmaes.control, control$infill.opt.cmaes.control, list())
  assertList(control$infill.opt.cmaes.control)

  control$infill.opt.ea.maxit = coalesce(opt.ea.maxit, control$infill.opt.ea.maxit, 500L)
  control$infill.opt.ea.maxit = asCount(control$infill.opt.ea.maxit)
  assertCount(control$infill.opt.ea.maxit, na.ok = FALSE, positive = TRUE)

  # cannot use coalsece here since we set the default later in checkstuff
  if (!is.null(opt.ea.mu)) {
    control$infill.opt.ea.mu = opt.ea.mu
  }

  control$infill.opt.ea.sbx.eta = coalesce(opt.ea.sbx.eta, control$infill.opt.ea.sbx.eta, 15)
  assertNumber(control$infill.opt.ea.sbx.eta, na.ok = FALSE, lower = 0)

  control$infill.opt.ea.sbx.p = coalesce(opt.ea.sbx.p, control$infill.opt.ea.sbx.p, 0.5)
  assertNumber(control$infill.opt.ea.sbx.p, na.ok = FALSE, lower = 0, upper = 1)

  control$infill.opt.ea.pm.eta = coalesce(opt.ea.pm.eta, control$infill.opt.ea.pm.eta, 15)
  assertNumber(control$infill.opt.ea.pm.eta, na.ok = FALSE, lower = 0)

  control$infill.opt.ea.pm.p = coalesce(opt.ea.pm.p, control$infill.opt.ea.pm.p, 0.5)
  assertNumber(control$infill.opt.ea.pm.p, na.ok = FALSE, lower = 0, upper = 1)

  control$infill.opt.ea.lambda = coalesce(opt.ea.lambda, control$infill.opt.ea.lambda, 1L)
  assertCount(control$infill.opt.ea.lambda, na.ok = FALSE)

  # FIXME: Don't use for now
  #control$infill.opt.multiobj.randomsearch.points = coalesce(opt.multiobj.randomsearch.points, control$infill.opt.multiobj.randomsearch.points)
  #control$infill.opt.multiobj.randomsearch.points = asCount(control$infill.opt.multiobj.randomsearch.points)
  #assertCount(control$infill.opt.multiobj.randomsearch.points, na.ok = FALSE, positive = TRUE)

  control$infill.opt.nsga2.popsize = coalesce(opt.nsga2.popsize, control$infill.opt.nsga2.popsize, 100L)
  control$infill.opt.nsga2.popsize = asCount(control$infill.opt.nsga2.popsize)
  assertCount(control$infill.opt.nsga2.popsize, na.ok = FALSE, positive = TRUE)
  if (control$infill.opt == "nsga2" && control$infill.opt.nsga2.popsize < control$propose.points)
    stop("Population size of nsga2 must be greater or equal than propose.points.")

  control$infill.opt.nsga2.generations = coalesce(opt.nsga2.generations, control$infill.opt.nsga2.generations, 50L)
  control$infill.opt.nsga2.generations = asCount(control$infill.opt.nsga2.generations)

  control$infill.opt.nsga2.cprob = coalesce(opt.nsga2.cprob, control$infill.opt.nsga2.cprob, 0.7)
  assertNumber(control$infill.opt.nsga2.cprob, lower = 0, upper = 1, na.ok = FALSE)

  control$infill.opt.nsga2.cdist = coalesce(opt.nsga2.cdist, control$infill.opt.nsga2.cdist, 5)
  assertNumber(control$infill.opt.nsga2.cdist, lower = 1e-16, na.ok = FALSE, finite = TRUE)

  control$infill.opt.nsga2.mprob = coalesce(opt.nsga2.mprob, control$infill.opt.nsga2.mprob, 0.2)
  assertNumber(control$infill.opt.nsga2.mprob, lower = 0, upper = 1, na.ok = FALSE)

  control$infill.opt.nsga2.mdist = coalesce(opt.nsga2.mdist, control$infill.opt.nsga2.mdist, 10)
  assertNumber(control$infill.opt.nsga2.mdist, lower = 1e-16, na.ok = FALSE, finite  = TRUE)

  return(control)
}

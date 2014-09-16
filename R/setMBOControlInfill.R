#' Extends mbo control object with infill criteria and infill optimizer options.
#'
#' @template arg_control
#' @param crit [\code{character(1)}]\cr
#'   How should infill points be rated. Possible parameter values are:
#'   \dQuote{mean}: Mean response.
#'   \dQuote{ei}: Expected improvement.
#'   \dQuote{aei}: Augmented expected improvement.
#'   \dQuote{lcb}: Lower confidence bound.
#'   \dQuote{multiFid}: Multifidelity: Expected improvement on different levels of the perfomance parameter defined in the \code{MBOMultiFidControl}.
#'   Alternatively, you may pass a function name as string.
#' @param crit.lcb.lambda [\code{numeric(1)}]\cr
#'   Lambda parameter for lower confidence bound infill criterion.
#'   Only used if \code{crit == "lcb"}, ignored otherwise.
#'   Default is 1.
#' @param opt [\code{character(1)}]\cr
#'   How should SINGLE points be proposed by using the surrogate model. Possible values are:
#'   \dQuote{focussearch}: In several iteration steps the parameter space is
#'   focused on an especial promising region according to infill criterion.
#'   \dQuote{cmaes}: Use CMAES to optimize infill criterion. If all CMAES runs fail, a random point is generated
#'   instead and a warning informs about it.
#'   \dQuote{ea}: Use an (mu+1) EA to optimize infill criterion.
#'   \dQuote{nsga2}: NSGA2 for multi obj. optimizationen. Needed for mspot.
#'   Default is \dQuote{focussearch}.
#'   Alternatively, you may pass a function name as string.
#' @param opt.restarts [\code{integer(1)}]\cr
#'   Number of independent restarts for optimizer of infill criterion.
#'   If \code{opt == "cmaes"} the first start point for the optimizer is always the
#'   currently best point in the design of already visited points.
#'   Subsequent restarts are started at random points.
#'   Default is 1.
#' @param opt.focussearch.maxit [\code{integer(1)}]\cr
#'   For \code{opt = "focussearch"}:
#'   Number of iteration to shrink local focus.
#'   Default is 5.
#' @param opt.focussearch.points [\code{integer(1)}]\cr
#'   For \code{opt = "focussearch"}:
#'   Number of points in each iteration of the focus search optimizer.
#'   Default is 10000.
#' @param opt.cmaes.control [\code{list}]\cr
#'   For \code{opt = "cmaes"}:
#'   Control argument for cmaes optimizer.
#'   Default is empty list.
#' @param opt.ea.maxit [\code{integer(1)}]\cr
#'   For \code{opt = "ea"}:
#'   Number of iterations / generations of EA.
#'   Default is 500.
#' @param opt.ea.mu [\code{integer(1)}]\cr
#'   For \code{opt = "ea"}:
#'   Population size of EA.
#'   Default is 10.
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
#'   For \code{opt.multicrit.method = "nsga2"}.
#'   Population size of nsga2.
#'   Default is 100.
#' @param opt.nsga2.generations [\code{numeric{1}}]\cr
#'   For \code{opt.multicrit.method = "nsga2"}.
#'   Number of populations for of nsga2.
#'   Default is 50.
#' @param opt.nsga2.cprob [\code{numeric{1}}]\cr
#'   For \code{opt.multicrit.method = "nsga2"}.
#'   nsga2 param. Default is 0.7.
#' @param opt.nsga2.cdist [\code{numeric{1}}]\cr
#'   For \code{opt.multicrit.method = "nsga2"}.
#'   nsga2 param. Default is 5.
#' @param opt.nsga2.mprob [\code{numeric{1}}]\cr
#'   For \code{opt.multicrit.method = "nsga2"}.
#'   nsga2 param. Default is 0.2.
#' @param opt.nsga2.mdist [\code{numeric{1}}]\cr
#'   For \code{opt.multicrit.method = "nsga2"}.
#'   nsga2 param. Default is 10.
#' @return [\code{\link{MBOControl}}].
#' @note See the other setMBOControl... functions and \code{makeMBOControl} for referenced arguments.
#' @seealso makeMBOControl
#' @export
setMBOControlInfill = function(control,
  crit = "mean", crit.lcb.lambda = 1,
  opt = "focussearch", opt.restarts = 1L,
  opt.focussearch.maxit = 5L, opt.focussearch.points = 10000L,
  opt.cmaes.control = list(),
  opt.ea.maxit = 500L, opt.ea.mu = 10L,
  opt.ea.sbx.eta = 15, opt.ea.sbx.p = 0.5,
  opt.ea.pm.eta = 15, opt.ea.pm.p = 0.5,
  opt.ea.lambda = 1L,
  #opt.multicrit.randomsearch.points = 50000L,
  opt.nsga2.popsize = 100L, opt.nsga2.generations = 50L,
  opt.nsga2.cprob = 0.7, opt.nsga2.cdist  = 5,
  opt.nsga2.mprob = 0.2, opt.nsga2.mdist = 10) {

  assertClass(control, "MBOControl")

  assertChoice(crit, choices = getSupportedInfillCritFunctions())
  assertNumeric(crit.lcb.lambda, len = 1L, any.missing = FALSE, lower = 0)
  assertChoice(opt, choices = getSupportedInfillOptFunctions())
  opt.restarts = asCount(opt.restarts)
  assertCount(opt.restarts, na.ok = FALSE)

  opt.focussearch.maxit = asCount(opt.focussearch.maxit)
  assertCount(opt.focussearch.maxit, na.ok = FALSE, positive = TRUE)
  opt.focussearch.points = asCount(opt.focussearch.points)
  assertCount(opt.focussearch.points, na.ok = FALSE, positive = TRUE)
  assertList(opt.cmaes.control)

  opt.ea.maxit = asCount(opt.ea.maxit)
  assertCount(opt.ea.maxit, na.ok = FALSE, positive = TRUE)
  opt.ea.mu = asCount(opt.ea.mu)
  assertCount(opt.ea.mu, na.ok = FALSE, positive = TRUE)
  assertNumber(opt.ea.sbx.eta, na.ok = FALSE, lower = 0)
  assertNumber(opt.ea.sbx.p, na.ok = FALSE, lower = 0, upper = 1)
  assertNumber(opt.ea.pm.eta, na.ok = FALSE, lower = 0)
  assertNumber(opt.ea.pm.p, na.ok = FALSE, lower = 0, upper = 1)
  assertCount(opt.ea.lambda, na.ok = FALSE)

  # FIXME: Don't use for now
  #opt.multicrit.randomsearch.points = asCount(opt.multicrit.randomsearch.points)
  #assertCount(opt.multicrit.randomsearch.points, na.ok = FALSE, positive = TRUE)

  opt.nsga2.popsize = asCount(opt.nsga2.popsize)
  assertCount(opt.nsga2.popsize, na.ok = FALSE, positive = TRUE)
  opt.nsga2.generations = asCount(opt.nsga2.generations)
  assertCount(opt.nsga2.generations, na.ok = FALSE, positive = TRUE)
  assertNumber(opt.nsga2.cprob, lower = 0, upper = 1, na.ok = FALSE)
  assertNumber(opt.nsga2.cdist, lower = 1e-16, na.ok = FALSE, finite = TRUE)
  assertNumber(opt.nsga2.mprob, lower = 0, upper = 1, na.ok = FALSE)
  assertNumber(opt.nsga2.mdist, lower = 1e-16, na.ok = FALSE, finite  = TRUE)


  control$infill.crit = crit
  control$infill.crit.lcb.lambda = crit.lcb.lambda
  control$infill.opt = opt
  control$infill.opt.restarts = opt.restarts
  control$infill.opt.focussearch.maxit = opt.focussearch.maxit
  control$infill.opt.focussearch.points = opt.focussearch.points
  control$infill.opt.cmaes.control = opt.cmaes.control
  control$infill.opt.ea.maxit = opt.ea.maxit
  control$infill.opt.ea.mu = opt.ea.mu
  control$infill.opt.ea.sbx.eta = opt.ea.sbx.eta
  control$infill.opt.ea.sbx.p = opt.ea.sbx.p
  control$infill.opt.ea.pm.eta = opt.ea.pm.eta
  control$infill.opt.ea.pm.p = opt.ea.pm.p
  control$infill.opt.ea.lambda = opt.ea.lambda
  #control$infill.opt.multicrit.randomsearch.points = opt.multicrit.randomsearch.points
  control$infill.opt.nsga2.popsize = opt.nsga2.popsize
  control$infill.opt.nsga2.generations = opt.nsga2.generations
  control$infill.opt.nsga2.cprob = opt.nsga2.cprob
  control$infill.opt.nsga2.cdist = opt.nsga2.cdist
  control$infill.opt.nsga2.mprob = opt.nsga2.mprob
  control$infill.opt.nsga2.mdist = opt.nsga2.mdist

  return(control)
}


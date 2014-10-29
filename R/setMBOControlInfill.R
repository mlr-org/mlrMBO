#' Extends mbo control object with infill criteria and infill optimizer options.
#'
#' @template arg_control
#' @param crit [\code{character(1)}]\cr
#'   How should infill points be rated. Possible parameter values are:
#'   \dQuote{mean}: Mean response.
#'   \dQuote{ei}: Expected improvement.
#'   \dQuote{aei}: Augmented expected improvement.
#'   \dQuote{eqi}: Expected quantile improvement.
#'   \dQuote{lcb}: Lower confidence bound.
#'   \dQuote{multiFid}: Multifidelity: Expected improvement on different levels of the perfomance parameter defined in the \code{MBOMultiFidControl}.
#'   Alternatively, you may pass a function name as string.
#' @param crit.eqi.beta [\code{numeric(1)}]\cr
#'   Beta parameter for expected quantile improvement criterion.
#'   Only used if \code{crit == "eqi"}, ignored otherwise.
#'   Default is 0.75.
#' @param crit.lcb.lambda [\code{numeric(1)}]\cr
#'   Lambda parameter for lower confidence bound infill criterion.
#'   Only used if \code{crit == "lcb"}, ignored otherwise.
#'   Default is 1.
# FIXME: does this only make sense for multicrit? or single crit too?
#' @param crit.lcb.pi [\code{numeric(1)}]\cr
#'   Probability-of-improvement value to determine the lambda parameter for lcb infill criterion.
#'   It is an alternative to set the trade-off between \dQuote{mean} and \dQuote{se}.
#'   Only used if \code{crit == "lcb"}, ignored otherwise.
#'   If specified, \code{crit.lcb.lambda == NULL} must hold.
#'   Default is \code{NULL}.
#' @param filter.proposed.points [\code{logical(1)}]\cr
#'   Design points located too close to each other can lead to
#'   numerical problems when using e.g. kriging as a surrogate model.
#'   This parameter activates or deactivates a heuristic to handle this issue.
#'   If \code{TRUE}, proposed points whose distance to design points or other current
#'   candidate points is smaller than \code{filter.proposed.points.tol}, are replaced by random points.
#'   If enabled, a logical column \dQuote{filter.replace} is added to the resulting \code{opt.path},
#'   so you can see whether such a replacement happened.
#'   Default is \code{FALSE}.
#' @param filter.proposed.points.tol [\code{numeric(1)}]\cr
#'   Tolerance value filtering of proposed points. We currently use a maximum metric
#'   to calculate the distance between points.
#'   Default is 0.0001.
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
  crit = NULL, crit.eqi.beta = 0.75, crit.lcb.lambda = 1, crit.lcb.pi = NULL,
  filter.proposed.points = NULL,
  filter.proposed.points.tol = NULL,
  opt = "focussearch", opt.restarts = NULL,
  opt.focussearch.maxit = NULL, opt.focussearch.points = NULL,
  opt.cmaes.control = NULL,
  opt.ea.maxit = NULL, opt.ea.mu = NULL,
  opt.ea.sbx.eta = NULL, opt.ea.sbx.p = NULL,
  opt.ea.pm.eta = NULL, opt.ea.pm.p = NULL,
  opt.ea.lambda = NULL,
  #opt.multicrit.randomsearch.points = 50000L,
  opt.nsga2.popsize = NULL, opt.nsga2.generations = NULL,
  opt.nsga2.cprob = NULL, opt.nsga2.cdist = NULL,
  opt.nsga2.mprob = NULL, opt.nsga2.mdist = NULL) {

  assertClass(control, "MBOControl")

  control$infill.crit = coalesce(crit, control$infill.crit, "mean")
  assertChoice(control$infill.crit, choices = getSupportedInfillCritFunctions())

  control$infill.crit.eqi.beta = coalesce(crit.eqi.beta, control$infill.crit.eqi.beta, 0.75)
  assertNumber(control$infill.crit.eqi.beta, na.ok = FALSE, lower = 0.5, upper = 1)
  
  # lambda value for lcb - either given, or set via given pi, the other one must be NULL!
  if (!is.null(crit.lcb.lambda) && !is.null(crit.lcb.pi))
    stop("Please specify either 'crit.lcb.lambda' or 'crit.lcb.pi' for the lcb crit, not both!")
  if (is.null(crit.lcb.pi))
    assertNumeric(crit.lcb.lambda, len = 1L, any.missing = FALSE, lower = 0)
  if (is.null(crit.lcb.lambda)) {
    assertNumeric(crit.lcb.pi, len = 1L, any.missing = FALSE, lower = 0, upper = 1)
    # This is the formula from TW diss for setting lambda.
    # Note, that alpha = -lambda, so we need the negative values
    crit.lcb.lambda = -qnorm(0.5 * crit.lcb.pi^(1 / control$number.of.targets))
  }
  control$infill.crit.lcb.lambda = coalesce(crit.lcb.lambda, control$infill.crit.lcb.lambda, 1)

  control$filter.proposed.points = coalesce(filter.proposed.points, control$filter.proposed.points, FALSE)
  assertFlag(control$filter.proposed.points)

  control$filter.proposed.points.tol = coalesce(filter.proposed.points.tol, control$filter.proposed.points.tol, 1e-4)
  assertNumber(control$filter.proposed.points.tol, na.ok = FALSE, lower = 0)

  control$infill.opt = coalesce(opt, control$infill.opt, "focussearch")
  assertChoice(control$infill.opt, choices = getSupportedInfillOptFunctions())

  control$infill.opt.restarts = coalesce(opt.restarts, control$infill.opt.restarts, 1L)
  control$infill.opt.restarts = asCount(control$infill.opt.restarts)
  assertCount(control$infill.opt.restarts, na.ok = FALSE)

  control$infill.opt.focussearch.maxit = coalesce(opt.focussearch.maxit, control$infill.opt.focussearch.maxit, 5L)
  control$infill.opt.focussearch.maxit = asCount(control$infill.opt.focussearch.maxit)
  assertCount(control$infill.opt.focussearch.maxit, na.ok = FALSE, positive = TRUE)

  control$infill.opt.focussearch.points = coalesce(opt.focussearch.points, control$infill.opt.focussearch.points, 10000L)

  control$infill.opt.focussearch.points = asCount(control$infill.opt.focussearch.points)
  assertCount(control$infill.opt.focussearch.points, na.ok = FALSE, positive = TRUE)

  control$infill.opt.cmaes.control = coalesce(opt.cmaes.control, control$infill.opt.cmaes.control, list())
  assertList(control$infill.opt.cmaes.control)

  control$infill.opt.ea.maxit = coalesce(opt.ea.maxit, control$infill.opt.ea.maxit, 500L)
  control$infill.opt.ea.maxit = asCount(control$infill.opt.ea.maxit)
  assertCount(control$infill.opt.ea.maxit, na.ok = FALSE, positive = TRUE)

  control$infill.opt.ea.mu = coalesce(opt.ea.mu, control$infill.opt.ea.mu, 10L)
  control$infill.opt.ea.mu = asCount(control$infill.opt.ea.mu)
  assertCount(control$infill.opt.ea.mu, na.ok = FALSE, positive = TRUE)

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
  #control$infill.opt.multicrit.randomsearch.points = coalesce(opt.multicrit.randomsearch.points, control$infill.opt.multicrit.randomsearch.points)
  #control$infill.opt.multicrit.randomsearch.points = asCount(control$infill.opt.multicrit.randomsearch.points)
  #assertCount(control$infill.opt.multicrit.randomsearch.points, na.ok = FALSE, positive = TRUE)
  
  control$infill.opt.nsga2.popsize = coalesce(opt.nsga2.popsize, control$infill.opt.nsga2.popsize, 100L)
  control$infill.opt.nsga2.popsize = asCount(control$infill.opt.nsga2.popsize)
  assertCount(control$infill.opt.nsga2.popsize, na.ok = FALSE, positive = TRUE)
  if (control$infill.opt.nsga2.popsize < control$propose.points)
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


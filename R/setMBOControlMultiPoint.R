#' Extends mbo control object with options for multipoint proposal.
#'
#' @template arg_control
#' @param method [\code{character(1)}]\cr
#'   Method used for proposal of multiple infill points, for parallel batch evaluation.
#'   Possible values are:
#'   \dQuote{lcb}: Proposes points by optimizing the lower confidence bound \dQuote{lcb} criterion,
#'   \code{propose.points} times. Each lambda value for \dQuote{lcb} is drawn randomly from an
#'   exp(1)-distribution, so do not define \code{infill.opt.lcb.lambda}.
#'   The optimizer for each proposal is configured in the same way as for the single point case,
#'   i. e., by specifying \code{infill.opt} and related stuff.
#'   \dQuote{multicrit}: Proposes points by evolutionary multicriteria optimization.
#'   The EA is a (mu+1) type of algorithm and runs for \code{multicrit.maxit} generations.
#'   The population size is set to \code{propose.points}.
#'   The selection criterion is \code{multicrit.selection}.
#'   \dQuote{cl}: Proposes points by constant liar strategy.
#'   Only meaningfull if \code{infill.crit == "lcb"}
#'   In the first step the kriging model is fitted based on the real data and the best point is calculated
#'   according to the regular EI-criterion.
#'   Then, the function value of the best point is simply guessed by the worst seen function evaluation.
#'   This lie is used to update the model in order to propose the subsequent point.
#'   The procedure is applied until the number of best points achieves \code{propose.points}.
#'   Default is \code{lcb}.
#' @param cl.lie [\code{function}]\cr
#'   Function used by constant liar method for lying. Default is \code{min}.
#' @param multicrit.objective [\code{character(1)}]\cr
#'   Objectives which are optimized in multicrit approach.
#'   Possible values are: \dQuote{mean.dist}, \dQuote{ei.dist}, \dQuote{mean.se}, \dQuote{mean.se.dist}.
#'   Default is \dQuote{ei.dist}.
#' @param multicrit.dist [\code{character(1)}]\cr
#'   Distance function used in multicrit EA.
#'   Possible values are: \dQuote{nearest.neigbor}, \dQuote{nearest.better}.
#'   Default is \dQuote{nearest.better}.
#FIXME: a link to the definition of nearest.better and nearest.neigbor?
#' @param multicrit.selection [\code{character(1)}]\cr
#'   Method used for selecting 1 element for removal from the population
#'   in each iteration of the multicriteria EA.
#'   Possible values are:
#'   \dQuote{hypervolume}: Non-dominated sorting + hypervolume contribution.
#'   \dQuote{crowdingdist}: Non-dominated sorting + crowding distance based ranking.
#'   \dQuote{first}: Non-dominated sorting + first objective of \code{multicrit.objective} as criterion.
#'   \dQuote{last}: Non-dominated sorting + last objective of \code{multicrit.objective} as criterion.
#'   Default is \code{hypervolume}.
#' @param multicrit.maxit [\code{character(1)}]\cr
#'   Number of generations for multicriteria EA.
#'   Default is 100.
#' @param multicrit.sbx.eta [\code{numeric(1)}]\cr
#'   Distance parameter of crossover distribution, see \code{\link[emoa]{sbx_operator}}.
#'   Default is 15.
#' @param multicrit.sbx.p [\code{numeric(1)}]\cr
#'   Probability of 1-point crossover, see \code{\link[emoa]{sbx_operator}}.
#'   Default is 1.
#' @param multicrit.pm.eta [\code{numeric(1)}]\cr
#'   Distance parameter of mutation distribution, see \code{\link[emoa]{pm_operator}}.
#'   Default is 15.
#' @param multicrit.pm.p [\code{numeric(1)}]\cr
#'   Probability of 1-point mutation, see \code{\link[emoa]{pm_operator}}.
#'   Default is 1.
#' @return [\code{\link{MBOControl}}].
#' @note See the other setMBOControl... functions and \code{makeMBOControl} for referenced arguments.
#' @seealso makeMBOControl
#' @export
setMBOControlMultiPoint = function(control,
  method = NULL,
  cl.lie = NULL,
  multicrit.objective = NULL,
  multicrit.dist = NULL,
  multicrit.selection = NULL,
  multicrit.maxit = NULL,
  multicrit.sbx.eta = NULL, multicrit.sbx.p = NULL,
  multicrit.pm.eta = NULL, multicrit.pm.p = NULL) {

  assertClass(control, "MBOControl")

  control$multipoint.method = coalesce(method, control$multipoint.method, "lcb")
  assertChoice(control$multipoint.method, choices = getSupportedMultipointInfillOptFunctions())

  # Workaround since coalesce cannot hande functions
  control$multipoint.cl.lie = if (!is.null(cl.lie)) {
    cl.lie
  } else if (!is.null(control$multipoint.cl.lie)) {
    control$multipoint.cl.lie
  } else {
    min
  }
  assertFunction(control$multipoint.cl.lie)

  control$multipoint.multicrit.objective = coalesce(multicrit.objective, control$multipoint.multicrit.objective, "ei.dist")
  assertChoice(control$multipoint.multicrit.objective, choices = c("mean.dist", "ei.dist", "mean.se", "mean.se.dist"))

  control$multipoint.multicrit.dist = coalesce(multicrit.dist, control$multipoint.multicrit.dist, "nearest.better")
  assertChoice(control$multipoint.multicrit.dist, choices = c("nearest.neighbor", "nearest.better"))

  control$multipoint.multicrit.selection = coalesce(multicrit.selection, control$multipoint.multicrit.selection, "hypervolume")
  assertChoice(control$multipoint.multicrit.selection, choices = c("hypervolume", "crowdingdist", "first", "last"))

  if (!is.null(multicrit.maxit)) {
    multicrit.maxit = asCount(multicrit.maxit) 
  }
  control$multipoint.multicrit.maxit = coalesce(multicrit.maxit, control$multipoint.multicrit.maxit, 100L)
  assertCount(control$multipoint.multicrit.maxit, na.ok = FALSE, positive = TRUE)

  control$multipoint.multicrit.sbx.eta = coalesce(multicrit.sbx.eta, control$multipoint.multicrit.sbx.eta, 15)
  assertNumber(control$multipoint.multicrit.sbx.eta, na.ok = FALSE, lower = 0)
 
  control$multipoint.multicrit.sbx.p = coalesce(multicrit.sbx.p, control$multipoint.multicrit.sbx.p, 1)
  assertNumber(control$multipoint.multicrit.sbx.p, na.ok = FALSE, lower = 0, upper = 1)
 
  control$multipoint.multicrit.pm.eta = coalesce(multicrit.pm.eta, control$multipoint.multicrit.pm.eta, 15)
  assertNumber(control$multipoint.multicrit.pm.eta, na.ok = FALSE, lower = 0)

  control$multipoint.multicrit.pm.p = coalesce(multicrit.pm.p, control$multipoint.multicrit.pm.p, 1)
  assertNumber(control$multipoint.multicrit.pm.p, na.ok = FALSE, lower = 0, upper = 1)

  # FIXME: this is currently a hidden option, also see multipoint_lcb.R
  control$lcb.min.dist = 1e-5

  return(control)
}

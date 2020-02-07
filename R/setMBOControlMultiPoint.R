#' @title Set multipoint proposal options.
#' @description
#' Extends an MBO control object with options for multipoint proposal.
#' @template arg_control
#' @param method [\code{character(1)}]\cr
#'   Method used for proposal of multiple infill points, for parallel batch evaluation.
#'   Possible values are:
#'   \dQuote{cb}: Proposes points by optimizing the confidence bound \dQuote{cb} criterion,
#'   \code{propose.points} times. Each lambda value for \dQuote{cb} is drawn randomly from an
#'   exp(1)-distribution, so do not define \code{infill.opt.cb.lambda}.
#'   The optimizer for each proposal is configured in the same way as for the single point case,
#'   i. e., by specifying \code{infill.opt} and related stuff.
#'   \dQuote{moimbo}: Proposes points by multi-objective infill criteria via evolutionary multi-objective optimization.
#'   The EA is a (mu+1) type of algorithm and runs for \code{moimbo.maxit} generations.
#'   The population size is set to \code{propose.points}.
#'   The selection criterion is \code{moimbo.selection}.
#'   If this method is selected the infill criterion in \code{setMBOInfillCrit} is ignored.
#'   \dQuote{cl}: Proposes points by constant liar strategy.
#'   Only meaningful if \code{infill.crit == "cb"}
#'   In the first step the kriging model is fitted based on the real data and the best point is calculated
#'   according to the regular EI-criterion.
#'   Then, the function value of the best point is simply guessed by the worst seen function evaluation.
#'   This lie is used to update the model in order to propose the subsequent point.
#'   The procedure is applied until the number of best points achieves \code{propose.points}.
#'   Default is \code{cb}.
#' @param cl.lie [\code{function}]\cr
#'   Function used by constant liar method for lying. Default is \code{min}.
#' @param moimbo.objective [\code{character(1)}]\cr
#'   Objectives which are optimized in multi-objective approach.
#'   Possible values are: \dQuote{mean.dist}, \dQuote{ei.dist}, \dQuote{mean.se}, \dQuote{mean.se.dist}.
#'   Default is \dQuote{ei.dist}.
#' @param moimbo.dist [\code{character(1)}]\cr
#'   Distance function used in multi-objective EA.
#'   Possible values are: \dQuote{nearest.neighbor}, \dQuote{nearest.better}.
#'   Default is \dQuote{nearest.better}.
#FIXME: a link to the definition of nearest.better and nearest.neighbor?
#' @param moimbo.selection [\code{character(1)}]\cr
#'   Method used for selecting 1 element for removal from the population
#'   in each iteration of the multi-objective EA.
#'   Possible values are:
#'   \dQuote{hypervolume}: Non-dominated sorting + hypervolume contribution.
#'   \dQuote{crowdingdist}: Non-dominated sorting + crowding distance based ranking.
#'   \dQuote{first}: Non-dominated sorting + first objective of \code{moimbo.objective} as criterion.
#'   \dQuote{last}: Non-dominated sorting + last objective of \code{moimbo.objective} as criterion.
#'   Default is \code{hypervolume}.
#' @param moimbo.maxit [\code{integer(1)}]\cr
#'   Number of generations for multi-objective EA.
#'   Default is 100.
#' @param moimbo.sbx.eta [\code{numeric(1)}]\cr
#'   Distance parameter of crossover distribution, see \code{\link[emoa]{sbx_operator}}.
#'   Default is 15.
#' @param moimbo.sbx.p [\code{numeric(1)}]\cr
#'   Probability of 1-point crossover, see \code{\link[emoa]{sbx_operator}}.
#'   Default is 1.
#' @param moimbo.pm.eta [\code{numeric(1)}]\cr
#'   Distance parameter of mutation distribution, see \code{\link[emoa]{pm_operator}}.
#'   Default is 15.
#' @param moimbo.pm.p [\code{numeric(1)}]\cr
#'   Probability of 1-point mutation, see \code{\link[emoa]{pm_operator}}.
#'   Default is 1.
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlMultiPoint = function(control,
  method = NULL,
  cl.lie = NULL,
  moimbo.objective = NULL,
  moimbo.dist = NULL,
  moimbo.selection = NULL,
  moimbo.maxit = NULL,
  moimbo.sbx.eta = NULL, moimbo.sbx.p = NULL,
  moimbo.pm.eta = NULL, moimbo.pm.p = NULL) {

  assertClass(control, "MBOControl")

  control$multipoint.method = coalesce(method, control$multipoint.method, "cb")
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

  control$multipoint.moimbo.objective = coalesce(moimbo.objective, control$multipoint.moimbo.objective, "ei.dist")
  assertChoice(control$multipoint.moimbo.objective, choices = c("mean.dist", "ei.dist", "mean.se", "mean.se.dist"))

  control$multipoint.moimbo.dist = coalesce(moimbo.dist, control$multipoint.moimbo.dist, "nearest.better")
  assertChoice(control$multipoint.moimbo.dist, choices = c("nearest.neighbor", "nearest.better"))

  control$multipoint.moimbo.selection = coalesce(moimbo.selection, control$multipoint.moimbo.selection, "hypervolume")
  assertChoice(control$multipoint.moimbo.selection, choices = c("hypervolume", "crowdingdist", "first", "last"))

  if (!is.null(moimbo.maxit)) {
    moimbo.maxit = asCount(moimbo.maxit)
  }
  control$multipoint.moimbo.maxit = coalesce(moimbo.maxit, control$multipoint.moimbo.maxit, 100L)
  assertCount(control$multipoint.moimbo.maxit, na.ok = FALSE, positive = TRUE)

  control$multipoint.moimbo.sbx.eta = coalesce(moimbo.sbx.eta, control$multipoint.moimbo.sbx.eta, 15)
  assertNumber(control$multipoint.moimbo.sbx.eta, na.ok = FALSE, lower = 0)

  control$multipoint.moimbo.sbx.p = coalesce(moimbo.sbx.p, control$multipoint.moimbo.sbx.p, 1)
  assertNumber(control$multipoint.moimbo.sbx.p, na.ok = FALSE, lower = 0, upper = 1)

  control$multipoint.moimbo.pm.eta = coalesce(moimbo.pm.eta, control$multipoint.moimbo.pm.eta, 15)
  assertNumber(control$multipoint.moimbo.pm.eta, na.ok = FALSE, lower = 0)

  control$multipoint.moimbo.pm.p = coalesce(moimbo.pm.p, control$multipoint.moimbo.pm.p, 1)
  assertNumber(control$multipoint.moimbo.pm.p, na.ok = FALSE, lower = 0, upper = 1)

  return(control)
}

#' @title Set options for handling noisy functions.
#' @description
#' Extends an MBO control object with options for handling noisy functions. 
#' @template arg_control
#' @param method [\code{character(1)}]\cr
#'   Which of the replication strategies should be used? Possible values are:
#'   \dQuote{fixed}: Every point is evaluated \code{instances} times. \cr
#'   \dQuote{incumbent}: Use an incumbent strategy as intensification strategy. 
#'    The size of the set of additional challengers (apart from the incumbent) can be specified in \code{incumbent.nchallengers}. \cr
#'   \dQuote{ocba}: Distribution replication budget according to OCBA. 
#'    The replication budget per iteration is specified in \code{ocba.budget},
#'    the initial number of iterations per parameter is specified in \code{ocba.initial}. \cr 
#' @param instances [\code{integer(1)}]\cr
#'   How many instances of one parameter will be calculated?
#' @param instance.param [\code{character(1)}]\cr
#'   What is the name of the function param that defines the instance?
#' @param self.replicating [\code{logical(1)}]\cr
#'   TRUE if the function returns a vector of noisy results for one input. Then \code{instances} specifies the length of the result we expect.
#' @param incumbent.nchallengers [\code{integer(1)}]\cr
#'   The size of the set of additional challengers (apart from the incumbent), defaults to \code{0}.
#' @param ocba.budget [\code{integer(1)}]\cr
#'   The budget that is allocated in each iteration per Optimal Computing Budget Allocation (OCBA) rule, defaults to 10.
#' @param ocba.initial [\code{integer(1)}]\cr
#'   The number of initial replications at each new point, defaults to \code{3}. 
#'   This needs to be larger than 1, since OCBA requires an initial variance estimate at each point.
#' @param instance.aggregation [\code{function}]\cr
#'   Should data be aggregated per instance? If yes, a function (e. g. mean) needs to be specified.  
#' @param identification.pcs [\code{numeric(1)}]\cr
#'   Minimum probability of correct selection. 
#'   If an identification budget is set, the algorithm spends this to achieve a minimum probability of correct selection of the final point.
#'   Defaults to \code{0.7}.
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlNoisy = function(control,
  method = NULL,
  instances = NULL,
  instance.param = NULL,
  instance.aggregation = NULL,
  self.replicating = NULL,
  incumbent.nchallengers = NULL, 
  ocba.budget = NULL,
  ocba.initial = NULL,
  identification.pcs = NULL) {

  assertClass(control, "MBOControl")
  control$noisy.method = coalesce(method, control$noisy.method, "fixed")
  assertChoice(control$noisy.method, choices = c("fixed", "incumbent", "ocba"))
  control$noisy.instances = assertInt(instances, lower = 1L, null.ok = TRUE, na.ok = FALSE) %??% control$noisy.instances %??% 1L
  control$noisy.self.replicating = assertFlag(self.replicating, null.ok = TRUE, na.ok = FALSE) %??% control$noisy.self.replicating %??% FALSE
  control$noisy.instance.param = assertString(instance.param, null.ok = TRUE, na.ok = TRUE) %??% control$noisy.instance.param %??% ifelse(control$noisy.self.replicating, "noisy.repl", NA_character_)
  control$noisy.instance.aggregation = assertClass(instance.aggregation, "function", null.ok = TRUE) %??% control$noisy.instance.aggregation 
  control$noisy.ocba.budget = assertInt(ocba.budget, lower = 1L, null.ok = TRUE, na.ok = FALSE) %??% control$noisy.ocba.budget %??% 3L
  control$noisy.ocba.initial = assertInt(ocba.initial, lower = 2L, null.ok = TRUE, na.ok = FALSE) %??% control$noisy.ocba.initial %??% 3L
  control$noisy.incumbent.nchallengers = assertInt(incumbent.nchallengers, lower = 0L, null.ok = TRUE, na.ok = FALSE) %??% control$noisy.incumbent.nchallengers %??% 0L
  control$noisy.identification.pcs = assertNumeric(identification.pcs, lower = 0, upper = 1, null.ok = TRUE) %??% control$noisy.identification.pcs %??% 0.7

  if (control$noisy.self.replicating && control$noisy.instance.param != "noisy.repl") {
    stop("You can not change the instance.param for self replicating functions.")
  }

  return(control)
}

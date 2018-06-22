#' @title Set multipoint proposal options.
#' @description
#' Extends an MBO control object with options for multipoint proposal.
#' @template arg_control
#' @param instances [\code{integer(1)}]\cr
#'   How many instances of one parameter will be calculated?
#' @param instance.param [\code{character(1)}]\cr
#'   What is the name of the function param that defines the instance?
#' @param self.replicating [\code{logical(1)}]\cr
#'   TRUE if the function returns a vector of noisy results for one input. Then \code{instances} specifies the length of the result we expect.
#' @param ocba.budget []
#' @param ocba.initial []
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlNoisy = function(control,
  method = NULL,
  instances = NULL,
  instance.param = NULL,
  self.replicating = NULL,
  ocba.budget = NULL) {

  assertClass(control, "MBOControl")
  control$noisy.method = coalesce(method, control$noisy.method, "fixed")
  assertChoice(control$noisy.method, choices = c("fixed", "incumbent", "ocba"))
  control$noisy.instances = assertInt(instances, lower = 1L, null.ok = TRUE, na.ok = FALSE) %??% control$noisy.instances
  control$noisy.self.replicating = assertFlag(self.replicating, null.ok = TRUE, na.ok = FALSE) %??% control$noisy.self.replicating %??% FALSE
  control$noisy.instance.param = assertString(instance.param, null.ok = TRUE, na.ok = TRUE) %??% control$noisy.instance.param %??% ifelse(control$noisy.self.replicating, "noisy.repl", NA_character_)
  control$noisy.ocba.budget = assertInt(ocba.budget, lower = 1L, null.ok = TRUE, na.ok = FALSE) %??% control$noisy.ocba.budget

  if (control$noisy.self.replicating && control$noisy.instance.param != "noisy.repl") {
    stop("You can not change the instance.param for self replicating functions.")
  }

  return(control)
}

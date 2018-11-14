#' @title Set multipoint proposal options.
#' @description
#' Extends an MBO control object with options for multipoint proposal.
#' @template arg_control
#' @param self.replicating [\code{logical(1)}]\cr
#'   TRUE if the function returns a vector of noisy results for one input. Then \code{instances} specifies the length of the result we expect.
#' @return [\code{\link{MBOControl}}].
#' @family MBOControl
#' @export
setMBOControlNoisy = function(control, self.replicating) {

  assertClass(control, "MBOControl")

  control$noisy.self.replicating = assertFlag(self.replicating %??% control$noisy.self.replicating %??% TRUE, na.ok = FALSE)
  control$noisy.instance.param = "noisy.repl"

  return(control)
}

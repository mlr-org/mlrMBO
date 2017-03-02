#' @title Perform an mbo run on a test function and and visualize what happens.
#'
#' @description
#' Usually used for 1D or 2D examples,
#' useful for figuring out how stuff works and for teaching purposes.
#' Currently only parameter spaces with numerical parameters are supported.
#' For visualization, run \code{plotExampleRun} on the resulting object.
#' What is displayed is documented here: \code{\link{plotExampleRun}}.
#'
#' @inheritParams mbo
#' @return [\code{MBOExampleRun}]
#' @export
exampleRun = function(fun, design = NULL, learner = NULL, control, show.info = FALSE, more.args = NULL) {
  control.mod = control
  control.mod$store.model.at = 1:1024L
  mbo.res = mbo(fun = fun, design = design, learner = learner, control = control.mod, show.info = show.info, more.args = more.args)
  list(mbo.res = mbo.res, obj.fun = fun)
}

#' @title Evaluates a given design
#' 
#' @description
#' Returns the design with the evaluated function values
#' 
#' @inheritParams mbo
#' 
#' @return [\code{data.frame}]
#' @export
preevaluateDesign = function(fun, design, control, show.info, more.args = list()) {
  opt.problem = makeOptProblem(
    fun = fun,
    par.set = smoof::getParamSet(fun),
    design = design,
    learner = NA,
    control = control,
    show.info = show.info,
    more.args = more.args)

  opt.state = makeOptState(opt.problem)
  evalMBODesign.OptState(opt.state)
  op = getOptStateOptPath(opt.state)
  cbind(getOptPathX(op), getOptPathY(op, drop = FALSE))
}
#' @title Perform an MBO run on a multi-criteria test function and and visualize what happens.
#'
#' @description
#' Only available for 2D -> 2D examples,
#' useful for figuring out how stuff works and for teaching purposes.
#' Currently only parameter spaces with numerical parameters are supported.
#' For visualization, run \code{autoplot} on the resulting object.
#' What is displayed is documented here: \code{\link{autoplot.MBOExampleRunMultiCrit}}.
#'
#' @param fun [\code{function}]\cr
#'   Target function. See \code{\link{mbo}} for details.
#' @template arg_parset
#' @param learner [\code{\link[mlr]{Learner}}]\cr
#'   See \code{\link{mbo}}.
#'   Default is mlr learner \dQuote{regr.km}, which is kriging from package
#'   DiceKriging, if all parameters are numeric. \code{nugget.estim} is set
#'   to \code{TRUE} depending on whether we have noisy observations or not.
#'   If a least one parameter is discrete the mlr learner \dQuote{regr.randomForest}
#'   from package RandomForest is used as the default.
#' @param control [\code{\link{MBOControl}}]\cr
#'   See \code{\link{mbo}}.
#' @param  points.per.dim [\code{integer}]\cr
#'   Number of (regular spaced) locations at which to
#'   sample the \code{fun} function per dimension.
#'   Default is 50.
#' @template arg_showinfo
#' @param nsga2.args [\code{list}]\cr
#'   Further arguments passed to the nsga2 call.
#'   Default is \code{list()}.
#' @param ref.point [\code{numeric(2)}]\cr
#'   Reference point to calculate the dominated hypervolume.
#'   Default is \code{c(11, 11)}.
#' @param ... [any]\cr
#'   Further arguments passed to the learner.
#' @return [\code{MBOExampleRunMultiCrit}]
exampleRunMultiCrit= function(fun, par.set, learner, control, points.per.dim = 50,
  show.info = TRUE, nsga2.args = list(), ref.point = c(11, 11), ...) {

  assertFunction(fun)
  assertClass(control, "MBOControl")
  assertClass(par.set, "ParamSet")
  assertNumeric(ref.point, lower = 0, finite = TRUE, any.missing = FALSE, len = 2L)
  par.types = getParamTypes(par.set)
  n.params = sum(getParamLengths(par.set))

  learner = checkLearner(learner, par.set, control, ...)

  assertLogical(show.info, len = 1L, any.missing = FALSE)
  ny = control$number.of.targets

  #FIXME: Only allow 2D -> 2D for the moment, since the plot function can only handle this case
  if (ny != 2L)
    stopf("exampleRunMultiCrit can only be applied for functions with 2 objectives, but you have %iD", ny)
  if (n.params != 2L)
    stopf("exampleRunMultiCrit can only be applied for functions with 2 parameters, but you have %iD", ny)

  control$store.model.at = 1:control$iters
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  names.y = control$y.name

  showInfo(show.info, "Running NSGA2 to approximate pareto front.")

  args = list(fun, idim = getParamNr(par.set, devectorize = TRUE), odim = ny,
    lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = 60L, generations = 100L)

  requirePackages("mco", why = "exampleRunMultiCrit")
  args = insert(args, nsga2.args)
  nsga2.res = do.call(nsga2, args)
  nsga2.paretoset = setColNames(as.data.frame(nsga2.res$par[nsga2.res$pareto.optimal, ]), names.x)
  nsga2.paretofront = setColNames(as.data.frame(nsga2.res$value[nsga2.res$pareto.optimal, ]), names.y)
  nsga2.hypervolume = dominated_hypervolume(t(nsga2.paretofront), ref.point)

  # show some info on console
  showInfo(show.info, "Performing MBO on function.")
  showInfo(show.info, "Initial design: %i. Sequential iterations: %i.", control$init.design.points, control$iters)
  showInfo(show.info, "Learner: %s. Settings:\n%s", learner$id, mlr:::getHyperParsString(learner))

  # run optimizer now
  res = mbo(fun, par.set, learner = learner, control = control, show.info = show.info)
  mbo.hypervolume = dominated_hypervolume(t(res$pareto.front), ref.point)

  makeS3Obj(c("MBOExampleRunMultiCrit"),
    par.set = par.set,
    n.params = n.params,
    par.types = par.types,
    names.x = names.x,
    names.y = names.y,
    learner = learner,
    mbo.hypervolume = mbo.hypervolume,
    control = control,
    nsga2.paretofront = nsga2.paretofront,
    nsga2.paretoset = nsga2.paretoset,
    nsga2.hypervolume = nsga2.hypervolume,
    mbo.res = res,
    points.per.dim = points.per.dim
  )
}

#' @export
print.MBOExampleRunMultiCrit = function(x, ...) {
  catf("MBOExampleRunMultiCrit")
  catf("Number of parameters          : %i", x$n.params)
  catf("Parameter names               : %s", collapse(x$names.x))
  catf("Parameter types               : %s", collapse(x$par.types))
  print(x$control)
  catf("Learner                       : %s", x$learner$id)
  catf("Learner settings:\n%s", mlr:::getHyperParsString(x$learner))
  catf("Hypervolume                   : %.4e", x$mbo.hypervolume)
  catf("NSGA2 Hypervolume (6000 FEs)  : %.4e", x$nsga2.hypervolume)
}



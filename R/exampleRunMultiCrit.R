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
#'   Reference point to calculate the dominated hypervolume for the fronts of nsga2 and mbo.
#'   Default is \code{NULL}, which means max(y_i) + 1 of the nsga2 front.
#' @param ... [any]\cr
#'   Further arguments passed to the learner.
#' @return [\code{MBOExampleRunMultiCrit}]
#' @export
exampleRunMultiCrit= function(fun, par.set, learner, control, points.per.dim = 50,
  show.info = NULL, nsga2.args = list(), ref.point = NULL, ...) {

  assertFunction(fun)
  assertClass(control, "MBOControl")
  assertClass(par.set, "ParamSet")
  if (!is.null(ref.point))
    assertNumeric(ref.point, lower = 0, finite = TRUE, any.missing = FALSE, len = 2L)
  par.types = getParamTypes(par.set)
  n.params = sum(getParamLengths(par.set))

  learner = checkLearner(learner, par.set, control, ...)

  if (is.null(show.info)) {
    show.info = getOption("mlrMBO.show.info", TRUE)
  }
  assertLogical(show.info, len = 1L, any.missing = FALSE)
  ny = control$number.of.targets

  # only allow 2D -> 2D for the moment, since the plot function can only handle this case
  if (ny != 2L)
    stopf("exampleRunMultiCrit can only be applied for functions with 2 objectives, but you have %iD", ny)
  if (n.params != 2L)
    stopf("exampleRunMultiCrit can only be applied for functions with 2 parameters, but you have %iD", n.params)

  control$store.model.at = 1:control$iters
  names.x = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  names.y = control$y.name

  showInfo(show.info, "Running NSGA2 to approximate pareto front.")

  # nsga2 interface does not allow maximization. fuck.
  mults = ifelse(control$minimize, 1, -1)
  if (!all(control$minimize)) {
    fun2 = function(x) fun(x) * mults
  } else {
    fun2 = fun
  }

  args = list(fun2, idim = getParamNr(par.set, devectorize = TRUE), odim = ny,
    lower.bounds = getLower(par.set), upper.bounds = getUpper(par.set),
    popsize = 60L, generations = 100L)

  requirePackages("mco", why = "exampleRunMultiCrit")
  args = insert(args, nsga2.args)
  nsga2.res = do.call(nsga2, args)
  nsga2.paretoset = setColNames(as.data.frame(nsga2.res$par[nsga2.res$pareto.optimal, ]), names.x)
  nsga2.paretofront = nsga2.res$value[nsga2.res$pareto.optimal, ]
  if (is.null(ref.point))
    ref.point = apply(nsga2.paretofront, 2, max) * mults  + 1
  # front: and back to orginal scale ...
  nsga2.paretofront = nsga2.paretofront %*% diag(mults)
  nsga2.paretofront = setColNames(as.data.frame(nsga2.paretofront), names.y)
  nsga2.hypervolume = getDominatedHV(nsga2.paretofront, ref.point, control$minimize)

  showInfo(show.info, "Performing MBO on function.")
  showInfo(show.info, "Initial design: %i. Sequential iterations: %i.", control$init.design.points, control$iters)
  showInfo(show.info, "Learner: %s. Settings:\n%s", learner$id, mlr:::getHyperParsString(learner))

  # run optimizer now
  res = mbo(fun, par.set, learner = learner, control = control, show.info = show.info)
  mbo.hypervolume = getDominatedHV(res$pareto.front, ref.point, control$minimize)

  # if we have 1 model per obj, predict whole space and approx pareto front, so we can see effect of model
  mbo.pred.grid.x = NULL
  mbo.pred.grid.mean = NULL
  mbo.pred.grid.lcb = NULL
  if (control$multicrit.method %in% c("dib", "mspot")) {
    mbo.pred.grid.x = generateGridDesign(par.set, resolution = points.per.dim)
    mbo.pred.grid.mean = vector("list", control$iters)
    for (iter in 1:control$iters) {
      mods = res$models[[iter]]
      ps = lapply(mods, predict, newdata = mbo.pred.grid.x)
      y1 = extractSubList(ps, c("data", "response"), simplify = "cols")
      y2 = setColNames(as.data.frame(y1), names.y)
      y2$.is.dom = isDominated(y1, control$minimize)
      mbo.pred.grid.mean[[iter]] = y2
      # if we have se estim, also calc LCB front
      if (mods[[1]]$learner$predict.type == "se") {
        z1 = extractSubList(ps, c("data", "se"), simplify = "cols")
        z1 = y1 - z1 %*% diag(mults)
        z2 = setColNames(as.data.frame(z1), names.y)
        z2$.is.dom = isDominated(z1, control$minimize)
        mbo.pred.grid.lcb[[iter]] = z2
      } else {
        mbo.pred.grid.lcb = NULL
      }
    }
  }
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
    points.per.dim = points.per.dim,
    mbo.pred.grid.x = mbo.pred.grid.x,
    mbo.pred.grid.mean = mbo.pred.grid.mean,
    mbo.pred.grid.lcb = mbo.pred.grid.lcb
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



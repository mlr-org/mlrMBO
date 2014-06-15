#' Creates a control object for MBO optimization.
#'
#' @param minimize [\code{logical}]\cr
#'   Should target functions be minimized? One value par target function.
#'   Default is \code{TRUE} for ever target function.
#' @param noisy [\code{logical(1)}]\cr
#'   Is the target function noisy?
#'   Default is \code{FALSE}.
#' @param number.of.targets [\code{integer(1)}]\cr
#'   How many target functions does the function have? Greater than one for
#'   multicriteria optimization, default ist 1.
#' @param multicrit.method [\code{character(1)}]\cr
#'   Which multicrit method should be used? At the moment only parego is
#'   supported, which is also the default.
#' @param init.design.points [\code{integer(1)}]\cr
#'   Number of points in inital design.
#'   Only used if no design is given in \code{mbo} function.
#'   Default is 20.
#' @param init.design.fun [\code{function}]\cr
#'   Function from package lhs for the sequential design.
#'   Possible are: \code{maximinLHS}, \code{randomLHS}, \code{geneticLHS},
#'   \code{improvedLHS}, \code{optAugmentLHS}, \code{optimumLHS}.
#'   Only used if no design is given in \code{mbo} function.
#'   Default is \code{maximinLHS}.
#' @param init.design.args [\code{list}]\cr
#'   List of further arguments passed to \code{init.design.fun}.
#'   Only used if no design is given in \code{mbo} function.
#'   Default is empty list.
#' @param iters [\code{integer(1)}]\cr
#'   Number of sequential optimization steps.
#'   Default is 10.
#' @param propose.points [\code{integer(1)}]\cr
#'   Number of proposed / really evaluated points each iteration.
#'   Default is 1.
#' @param infill.crit [\code{character(1)}]\cr
#'   How should infill points be rated. Possible parameter values are:
#'   \dQuote{mean}: Mean response.
#'   \dQuote{ei}: Expected improvement.
#'   \dQuote{aei}: Augmented expected improvement.
#'   \dQuote{lcb}: Lower confidence bound.
#'   Alternatively, you may pass a function name as string.
#' @param infill.crit.lcb.lambda [\code{numeric(1)}]\cr
#'   Lambda parameter for lower confidence bound infill criterion.
#'   Only used if \code{infill.crit == "lcb"}, ignored otherwise.
#'   Deafult is 1.
#' @param infill.opt [\code{character(1)}]\cr
#'   How should SINGLE points be proposed by using the surrogate model. Possible values are:
#'   \dQuote{focussearch}: In several iteration steps the parameter space is
#'   focused on an especial promising region according to infill criterion.
#'   \dQuote{cmaes}: Use CMAES to optimize infill criterion. If all CMAES runs fail, a random point is generated
#'   instead and a warning informs about it.
#'   \dQuote{ea}: Use an (mu+1) EA to optimize infill criterion.
#'   Default is \dQuote{focussearch}.
#'   Alternatively, you may pass a function name as string.
#' @param infill.opt.restarts [\code{integer(1)}]\cr
#'   Number of independent restarts for optimizer of infill criterion.
#'   If \code{infill.opt == "cmaes"} the first start point for the optimizer is always the
#'   currently best point in the design of already visited points.
#'   Subsequent restarts are started at random points.
#'   Default is 1.
#' @param infill.opt.focussearch.maxit [\code{integer(1)}]\cr
#'   For \code{infill.opt = "focussearch"}:
#'   Number of iteration to shrink local focus.
#'   Default is 5.
#' @param infill.opt.focussearch.points [\code{integer(1)}]\cr
#'   For \code{infill.opt = "focussearch"}:
#'   Number of points in each iteration of the focus search optimizer.
#'   Default is 10000.
#' @param infill.opt.cmaes.control [\code{list}]\cr
#'   For \code{infill.opt = "cmaes"}:
#'   Control argument for cmaes optimizer.
#'   Default is empty list.
#' @param infill.opt.ea.maxit [\code{integer(1)}]\cr
#'   For \code{infill.opt = "ea"}:
#'   Number of iterations / generations of EA.
#'   Default is 500.
#' @param infill.opt.ea.mu [\code{integer(1)}]\cr
#'   For \code{infill.opt = "ea"}:
#'   Population size of EA.
#'   Default is 10.
#' @param infill.opt.ea.pm.eta [\code{numeric(1)}]\cr
#'   For \code{infill.opt = "ea"}:
#'   Distance parameter of mutation distribution, see \code{\link[emoa]{pm_operator}}.
#'   Default is 15.
#' @param infill.opt.ea.pm.p [\code{numeric(1)}]\cr
#'   For \code{infill.opt = "ea"}:
#'   Probability of 1-point mutation, see \code{\link[emoa]{pm_operator}}.
#'   Default is 0.5.
#' @param infill.opt.ea.sbx.eta [\code{numeric(1)}]\cr
#'   For \code{infill.opt = "ea"}:
#'   Distance parameter of crossover distribution , see \code{\link[emoa]{sbx_operator}}.
#'   Default is 15.
#' @param infill.opt.ea.sbx.p [\code{numeric(1)}]\cr
#'   For \code{infill.opt = "ea"}:
#'   Probability of 1-point crossover, see \code{\link[emoa]{sbx_operator}}.
#'   Default is 0.5.
#' @param infill.opt.ea.lambda [\code{numeric{1}}]\cr
#'   For \code{infill.opt.ea = "ea"}.
#'   Number of children generated in each generation.
#'   Default is 1.
#' @param feature.impute [\code{character(1)}]\cr
#'   Method used for imputing features, which can / will be necessary for dependent parameters.
#'   Possible values are:
#'   \dQuote{up}: Numeric vars are imputed with 2 * upper bound.
#'   \dQuote{median}: Imputes NAs with median and add logical is.na variable.
#' @param multipoint.method [\code{character(1)}]\cr
#'   Method used for proposal of multiple infill points, for parallel batch evaluation.
#'   Possible values are:
#'   \dQuote{lcb}: Proposes points by optimizing the lower confidence bound \dQuote{lcb} criterion,
#'   \code{propose.points} times. Each lambda value for \dQuote{lcb} is drawn randomly from an
#'   exp(1)-distribution, so do not define \code{infill.opt.lcb.lambda}.
#'   The optimizer for each proposal is configured in the same way as for the single point case,
#'   i.e., by specifying \code{infill.opt} and related stuff.
#'   \dQuote{multicrit}: Proposes points by evolutionary multicriteria optimization.
#'   The EA is a (mu+1) type of algorithm and runs for \code{multipoint.multicrit.maxit} generations.
#'   The population size is set to \code{propose.points}.
#'   The selection criterion is \code{multipoint.multicrit.selection}.
#'	 \dQuote{cl}: Proposes points by constant liar strategie.
#'	 Only meaningfull if \code{infill.crit == "lcb"}
#'   In the first step the kriging model is fitted based on the real data and the best point is calculated according to the regular EI-criterion.
#'   Then, the function value of the best point is simply guessed by the worst seen function evaluation.
#'   This lie is used to update the model in order to propose the subsequent point.
#'   The procedure is applied until the number of best points achieves \code{propose.points}.
#'   Default is \code{lcb}
#' @param multipoint.multicrit.objective [\code{character(1)}]\cr
#'   Objectives which are optimized in multicrit approach.
#'   Possible values are: \dQuote{mean.dist}, \dQuote{ei.dist}, \dQuote{mean.se}, \dQuote{mean.se.dist}.
#'   Default is \dQuote{ei.dist}.
#' @param multipoint.multicrit.dist [\code{character(1)}]\cr
#'   Distance function used in multicrit EA.
#'   Possible values are: \dQuote{nearest.neigbor}, \dQuote{nearest.better}.
#'   Default is \dQuote{nearest.better}.
#FIXME: a link to the definition of nearest.better and nearest.neigbor?
#' @param multipoint.multicrit.selection [\code{character(1)}]\cr
#'   Method used for selecting 1 element for removal from the population
#'   in each iteration of the multicriteria EA.
#'   Possible values are:
#'   \dQuote{hypervolume}: Non-dominated sorting + hypervolume contribution.
#'   \dQuote{crowdingdist}: Non-dominated sorting + crowding distance based ranking.
#'   \dQuote{first}: Non-dominated sorting + first objective of \code{multipoint.multicrit.objective} as criterion.
#'   \dQuote{last}: Non-dominated sorting + last objective of \code{multipoint.multicrit.objective} as criterion.
#'   Default is \code{hypervolume}
#' @param multipoint.multicrit.maxit [\code{character(1)}]\cr
#'   Number of generations for multicriteria EA.
#'   Default is 100.
#' @param multipoint.multicrit.sbx.eta [\code{numeric(1)}]\cr
#'   Distance parameter of crossover distribution, see \code{\link[emoa]{sbx_operator}}.
#'   Default is 15.
#' @param multipoint.multicrit.sbx.p [\code{numeric(1)}]\cr
#'   Probability of 1-point crossover, see \code{\link[emoa]{sbx_operator}}.
#'   Default is 1.
#' @param multipoint.multicrit.pm.eta [\code{numeric(1)}]\cr
#'   Distance parameter of mutation distribution, see \code{\link[emoa]{pm_operator}}.
#'   Default is 15.
#' @param multipoint.multicrit.pm.p [\code{numeric(1)}]\cr
#'   Probability of 1-point mutation, see \code{\link[emoa]{pm_operator}}.
#'   Default is 1
#' @param parego.s [\code{integer(1)}]\cr
#'   Parameter of parego - controls the number of weighting vectors. The default
#'   depends on \code{number.of.targets} and leads to 100000 different possible
#'   weight vectors. The defaults for (2, 3, 4, 5, 6) dimensions are (100000,
#'   450, 75, 37, 23) and 10 for higher dimensions.
#' @param parego.rho [\code{numeric(1)}]\cr
#'   Parameter of parego - factor for Tchebycheff function. Default 0.05 as
#'   suggested in parego paper.
#' @param parego.sample.more.weights [\code{numeric(1)}]\cr
#'   In each iteration \code{parego.sample.more.weights} * \code{propose.points}
#'   are sampled and the weights with maximum distance to each other are chosen.
#'   Default is 1, if only 1 point is proposed each iteration, otherwise 5.
#' @param parego.use.margin.points [\code{logical}]\cr
#'   For each target function: Should the weight vector (0, ..., 0, 1, 0, ..., 0),
#'   i.e. the weight vector with only 0 and a single 1 at the i.th position for
#'   the i.th target function, be drawn with probability 1? Number of TRUE entries
#'   must be less or equal to \code{propose.points}
#'   Default is not to do this.
#' @param final.method [\code{character(1)}]\cr
#'   How should the final point be proposed. Possible values are:
#'   \dQuote{best.true.y}: Return best point ever visited according to true value of target function.
#'   Can be bad if target function is noisy.
#'   \dQuote{last.proposed}: Return the last point proposed by the model.
#'   \dQuote{best.predicted}: Use the final model to predict all points ever visited and use the best one.
#'   This might average-out noisy function values.
#'   Default is: \dQuote{best.true.y}.
#' @param final.evals [\code{integer(1)}]\cr
#'   How many target function evals should be done at final point to reduce noise?
#'   Default is 0.
#' @param y.name [\code{character}]\cr
#'   Vector for names of y-columns for target values in optimization path.
#'   Default is \dQuote{y_i}, i = 1, ..., number.of.targets.
#' @param impute.y.fun [\code{function(x, y, opt.path), ...)}*]\cr
#'   Functions that gets triggered if your objective evaluation produced
#'   a) an exception b) a return object of invalid type c) a numeric vector that
#'   contains \code{NA}, \code{NaN}, \code{Inf}.
#'   You now have a chance to handle this. You are expected to return a numeric vector
#'   of the correct length with concrete values.
#'   The optimization path will show some information whether y-values where imputed
#'   and what the original, faulty object was.
#'   \code{x} is the current x-value, \code{y} the current (invalid) y-object (or an error object)
#'   and \code{opt.path} the current optimization path.
#'   Default is \code{NULL} which means to stop if the objective function did not produce the desired
#'   result.
#' @param suppress.eval.errors [\code{logical(1)}]\cr
#'   Should reporting of error messages during target function evaluations be suppressed?
#'   Only used if \code{impute.errors} is \code{TRUE}.
#'   Default is \code{TRUE}.
#' @param save.on.disk.at [\code{integer}] \cr
#'   Sequential optimization iteration when the actual state should be saved
#'   on disk. Iteration 0 denotes the initial design. If the optimization
#'   stops with an crucial error, it can be restarted with this file via the
#'   function \link{restartSavedMBO}. Default is NULL. Make sure to specify
#'   \code{save.file.path}.
#' @param save.file.path [\code{character(1)}] \cr
#'   If \code{save.on.disk.at} is used, this is the name of the file where the data
#'   will be saved. Default is NULL.
#' @param save.model.at [\code{integer}]\cr
#'   Sequential optimization iterations when the model should be saved.
#'   Iteration 0 is the model fit for the initial design.
#'   Default is \code{iters}.
#' @param resample.at [\code{integer}]\cr
#'   At which iterations should the model be resampled and assessed?
#'   Iteration 0 does some resampling on the initial design.
#'   Default is none.
#' @param resample.desc [\code{\link[mlr]{ResampleDesc}}]\cr
#'   How should the model be resampled?
#'   Default is 10-fold CV.
#' @param resample.measures [list of \code{\link[mlr]{Measure}}]\cr
#'   Performance measures to assess model with during resampling.
#'   Default is \code{\link[mlr]{mse}}.
#' @param on.learner.error [\code{character(1)}]\cr
#'   See [\code{\link[mlr]{configureMlr}}].
#'   Default is \dQuote{stop}.
#' @param show.learner.output [\code{logical(1)}]\cr
#'   See [\code{\link[mlr]{configureMlr}}].
#'   Default is \code{FALSE}.
#' @param output.num.format [\code{logical(1)}]\cr
#'   Format string for the precision of the numeric output of mbo.
#' @return [\code{\link{MBOControl}}].
#' @aliases MBOControl
#' @export
makeMBOControl = function(number.of.targets = 1L,
  minimize = rep(TRUE, number.of.targets), multicrit.method = "parego", noisy = FALSE,
  init.design.points = 20L, init.design.fun = maximinLHS, init.design.args = list(),
  iters = 10L, propose.points = 1L, infill.crit = "mean", infill.crit.lcb.lambda = 1,
  infill.opt = "focussearch", infill.opt.restarts = 1L,
  infill.opt.focussearch.maxit = 5L, infill.opt.focussearch.points = 10000L,
  infill.opt.cmaes.control = list(),
  infill.opt.ea.maxit = 500L, infill.opt.ea.mu = 10L,
  infill.opt.ea.sbx.eta = 15, infill.opt.ea.sbx.p = 0.5,
  infill.opt.ea.pm.eta = 15, infill.opt.ea.pm.p = 0.5,
  infill.opt.ea.lambda = 1L,
  feature.impute = "up",
  multipoint.method = "lcb",
  multipoint.multicrit.objective = "ei.dist",
  multipoint.multicrit.dist = "nearest.better",
  multipoint.multicrit.selection = "hypervolume",
  multipoint.multicrit.maxit = 100L,
  multipoint.multicrit.sbx.eta = 15, multipoint.multicrit.sbx.p = 1,
  multipoint.multicrit.pm.eta = 15, multipoint.multicrit.pm.p = 1,
  parego.s, parego.rho = 0.05,
  parego.use.margin.points = rep(FALSE, number.of.targets),
  parego.sample.more.weights = 5L,
  final.method = "best.true.y", final.evals = 0L,
  y.name = "y",
  impute.y.fun = NULL,
  suppress.eval.errors = TRUE,
  save.on.disk.at = NULL,
  save.file.path = "", save.model.at = iters,
  resample.at = integer(0), resample.desc = makeResampleDesc("CV", iter = 10), resample.measures = list(mse),
  on.learner.error = "warn", show.learner.output = FALSE,
  output.num.format = "%.3g"
) {

  requirePackages("lhs", "makeMBOControl")

  number.of.targets = convertInteger(number.of.targets)
  checkArg(number.of.targets, "integer", len = 1L, min = 1L, na.ok = FALSE)
  checkArg(multicrit.method, choices = c("parego"))
  checkArg(minimize, "logical", len = number.of.targets, na.ok = FALSE)
  checkArg(noisy, "logical", len = 1L, na.ok = FALSE)

  init.design.points = convertInteger(init.design.points)
  checkArg(init.design.points, "integer", len = 1L, na.ok = FALSE, lower = 4L)
  checkArg(init.design.fun, "function")
  checkArg(init.design.args, "list")

  iters = convertInteger(iters)
  checkArg(iters, "integer", len = 1L, na.ok = FALSE, lower = 1L)
  propose.points = convertInteger(propose.points)
  checkArg(propose.points, "integer", len = 1L, na.ok = FALSE, lower = 1L)


  # FIXME: BB: DO NOT FUCKING TOUCH THIS!
  # checkArg(infill.crit, choices = getSupportedInfillCritFunctions())
  checkArg(infill.crit.lcb.lambda, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  checkArg(infill.opt, choices = getSupportedInfillOptFunctions())
  infill.opt.restarts = convertInteger(infill.opt.restarts)
  checkArg(infill.opt.restarts, "integer", len = 1L, na.ok = FALSE)

  infill.opt.focussearch.maxit = convertInteger(infill.opt.focussearch.maxit)
  checkArg(infill.opt.focussearch.maxit, "integer", len = 1L, na.ok = FALSE, lower = 1L)
  infill.opt.focussearch.points = convertInteger(infill.opt.focussearch.points)
  checkArg(infill.opt.focussearch.points, "integer", len = 1L, na.ok = FALSE, lower = 1L)
  checkArg(infill.opt.cmaes.control, "list")

  infill.opt.ea.maxit = convertInteger(infill.opt.ea.maxit)
  checkArg(infill.opt.ea.maxit, "integer", len = 1L, na.ok = FALSE, lower = 1L)
  infill.opt.ea.mu = convertInteger(infill.opt.ea.mu)
  checkArg(infill.opt.ea.mu, "integer", len = 1L, na.ok = FALSE, lower = 1L)
  checkArg(infill.opt.ea.sbx.eta, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  checkArg(infill.opt.ea.sbx.p, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1)
  checkArg(infill.opt.ea.pm.eta, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  checkArg(infill.opt.ea.pm.p, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1)
  checkArg(infill.opt.ea.lambda, "integer", len = 1L, na.ok = FALSE, lower = 1L)

  checkArg(feature.impute, choices = c("up", "median"))

  checkArg(multipoint.method, choices = getSupportedMultipointInfillOptFunctions())
  checkArg(multipoint.multicrit.objective, choices = c("mean.dist", "ei.dist", "mean.se", "mean.se.dist"))
  checkArg(multipoint.multicrit.selection, choices = c("hypervolume", "crowdingdist", "first", "last"))
  checkArg(multipoint.multicrit.dist, choices = c("nearest.neighbor", "nearest.better"))
  multipoint.multicrit.maxit = convertInteger(multipoint.multicrit.maxit)
  checkArg(multipoint.multicrit.maxit, "integer", len = 1L, na.ok = FALSE, lower = 0L)
  checkArg(multipoint.multicrit.sbx.eta, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  checkArg(multipoint.multicrit.sbx.p, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1)
  checkArg(multipoint.multicrit.pm.eta, "numeric", len = 1L, na.ok = FALSE, lower = 0)
  checkArg(multipoint.multicrit.pm.p, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1)

  if (missing(parego.s))
    parego.s = switch(min(number.of.targets, 7), 1L, 100000L, 450L, 75L, 37L, 23L, 10L)

  if (number.of.targets > 1L) {
    parego.s = convertInteger(parego.s)
    checkArg(parego.s, "integer", len = 1L, na.ok = FALSE, lower = 1)
    checkArg(parego.rho, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1)
    if (propose.points == 1L)
      parego.sample.more.weights = 1L
    parego.sample.more.weights = convertInteger(parego.sample.more.weights)
    checkArg(parego.sample.more.weights, "numeric", len = 1L, na.ok = FALSE, lower = 1)
    checkArg(parego.use.margin.points, "logical", len = number.of.targets, na.ok = FALSE, lower = 1)
    if (sum(parego.use.margin.points) > propose.points)
      stopf("Can't use %s margin points when only proposing %s points each iteration.",
        sum(parego.use.margin.points), propose.points)
    number.of.weights = choose(parego.s + number.of.targets - 1, number.of.targets - 1)
    if (parego.sample.more.weights * propose.points > number.of.weights)
      stop("Trying to sample more weights than exists. Increase parego.s or decrease number of weights.")
  }

  if (!is.null(impute.y.fun))
    checkArg(impute.y.fun, formals = c("x", "y", "opt.path"))

  checkArg(suppress.eval.errors, "logical", len = 1L, na.ok = FALSE)

  checkArg(final.method, choices = c("last.proposed", "best.true.y", "best.predicted"))
  final.evals = convertInteger(final.evals)
  checkArg(final.evals, "integer", len = 1L, na.ok = FALSE, lower = 0L)

  if (number.of.targets > 1 && length(y.name) == 1 && y.name == "y")
    y.name = paste("y", 1:number.of.targets, sep = "_")
  checkArg(y.name, "character", len = number.of.targets, na.ok = FALSE)

  if (!is.null(save.on.disk.at)) {
    save.on.disk.at = convertInteger(save.on.disk.at)
    checkArg(save.on.disk.at, "integer")
    if (save.file.path == "") {
      stopf("You must specify a file for saving.")
    } else {
      checkArg(save.file.path, "character", len = 1)
      # FIXME: How to check if save.file.path is correct for saving?
      # This does not look like a cool way to do it.
      tmp = try(save(save.file.path, file = save.file.path))
      if (inherits(tmp, "try-error"))
        stopf("Please specify a correct save.file.path.")
    }
  }
  if (is.null(save.on.disk.at) & save.file.path != "") {
    stopf("You specified a save.file.path, but you will never use it. You should specify iterations for saving.")
  }

  save.model.at = convertIntegers(save.model.at)
  checkArg(save.model.at, "integer", na.ok = FALSE, lower = 0L, upper = iters)

  if (length(resample.at) > 0) {
    resample.at = convertIntegers(resample.at)
    checkArg(resample.at, "integer", na.ok = FALSE, lower = 0L, upper = iters)
  } else {
    resample.at = integer(0)
  }
  checkArg(resample.desc, "ResampleDesc")
  checkArg(resample.measures, "list")

  checkArg(on.learner.error, choices = c("warn", "quiet", "stop"))
  checkArg(show.learner.output, "logical", len = 1L, na.ok = FALSE)
  checkArg(output.num.format, "character", len = 1L, na.ok = FALSE)

  makeS3Obj("MBOControl",
    minimize = minimize,
    noisy = noisy,
    number.of.targets = number.of.targets,
    multicrit.method = multicrit.method,
    init.design.points = init.design.points,
    init.design.fun = init.design.fun,
    init.design.args = init.design.args,
    iters = iters,
    propose.points = propose.points,
    infill.crit = infill.crit,
    infill.crit.lcb.lambda = infill.crit.lcb.lambda,
    infill.opt = infill.opt,
    infill.opt.restarts = infill.opt.restarts,
    infill.opt.focussearch.maxit = infill.opt.focussearch.maxit,
    infill.opt.focussearch.points = infill.opt.focussearch.points,
    infill.opt.cmaes.control = infill.opt.cmaes.control,
    infill.opt.ea.maxit = infill.opt.ea.maxit,
    infill.opt.ea.mu = infill.opt.ea.mu,
    infill.opt.ea.sbx.eta = infill.opt.ea.sbx.eta,
    infill.opt.ea.sbx.p = infill.opt.ea.sbx.p,
    infill.opt.ea.pm.eta = infill.opt.ea.pm.eta,
    infill.opt.ea.pm.p = infill.opt.ea.pm.p,
    infill.opt.ea.lambda = infill.opt.ea.lambda,
    feature.impute = feature.impute,
    multipoint.method = multipoint.method,
    multipoint.multicrit.objective = multipoint.multicrit.objective,
    multipoint.multicrit.dist = multipoint.multicrit.dist,
    multipoint.multicrit.selection = multipoint.multicrit.selection,
    multipoint.multicrit.maxit = multipoint.multicrit.maxit,
    multipoint.multicrit.sbx.eta = multipoint.multicrit.sbx.eta,
    multipoint.multicrit.sbx.p = multipoint.multicrit.sbx.p,
    multipoint.multicrit.pm.eta = multipoint.multicrit.pm.eta,
    multipoint.multicrit.pm.p = multipoint.multicrit.pm.p,
    parego.s = parego.s,
    parego.rho = parego.rho,
    parego.use.margin.points = parego.use.margin.points,
    parego.sample.more.weights = parego.sample.more.weights,
    final.method = final.method,
    final.evals = final.evals,
    y.name = y.name,
    impute.y.fun = impute.y.fun,
    suppress.eval.errors = suppress.eval.errors,
    save.on.disk.at = save.on.disk.at,
    save.file.path = save.file.path,
    save.model.at = save.model.at,
    resample.desc = resample.desc,
    resample.at = resample.at,
    resample.measures = resample.measures,
    on.learner.error = on.learner.error,
    show.learner.output = show.learner.output,
    output.num.format = output.num.format
  )
}

#' Print mbo control object.
#'
#' @param x [\code{\link{MBOControl}}]\cr
#'   Control object.
#' @param ... [any]\cr
#'   Not used.
#' @export
print.MBOControl = function(x, ...) {
  catf("Objective                   : %s",
    collapsef("%s = %s!", x$y.name, ifelse(x$minimize, "min", "max"), sep = "; "))
  catf("Function type               : %s",  ifelse(x$noisy, "noisy", "deterministic"))
  catf("Init. design                : %i points", x$init.design.points)
  catf("Iterations                  : %i", x$iters)
  catf("Points proposed per iter:   : %i", x$propose.points)
  if (x$propose.points == 1) {
  catf("Infill criterion            : %s", x$infill.crit)
  catf("Infill optimizer            : %s", x$infill.opt)
  catf("Infill optimizer restarts   : %i", x$infill.opt.restarts)
  } else {
  catf("Multipoint method           : %s", x$multipoint.method)
  }
  catf("Final point by              : %s", x$final.method)
}

\name{makeMBOControl}
\alias{makeMBOControl}
\alias{MBOControl}
\title{Creates a control object for MBO optimization.}
\usage{
  makeMBOControl(minimize = TRUE, noisy = FALSE,
    init.design.points = 20L, init.design.fun = maximinLHS,
    init.design.args = list(), iters = 10L,
    propose.points = 1L, infill.crit = "mean",
    infill.crit.lcb.lambda = 1, infill.opt = "random",
    infill.opt.restarts = 1L, infill.opt.random.maxit = 5L,
    infill.opt.random.points = 10000L,
    infill.opt.cmaes.control = list(),
    infill.opt.es.maxit = 500L, infill.opt.es.mu = 10L,
    infill.opt.es.eta = 15, infill.opt.es.p = 0.5,
    feature.impute = "up", multipoint.method = "lcb",
    multipoint.multicrit.objective = "ei.dist",
    multipoint.multicrit.dist = "nearest.better",
    multipoint.multicrit.selection = "hypervolume",
    multipoint.multicrit.maxit = 100L,
    final.method = "best.true.y", final.evals = 0L,
    y.name = "y", impute, impute.errors = FALSE,
    suppress.eval.errors = TRUE, save.model.at = iters,
    resample.at = integer(0),
    resample.desc = makeResampleDesc("CV", iter = 10),
    resample.measures = list(mse),
    on.learner.error = "warn", show.learner.output = FALSE)
}
\arguments{
  \item{minimize}{[\code{logical(1)}]\cr Should target
  function be minimized?  Default is \code{TRUE}.}

  \item{noisy}{[\code{logical(1)}]\cr Is the target
  function noisy?  Default is \code{FALSE}.}

  \item{init.design.points}{[\code{integer(1)}]\cr Number
  of points in inital design.  Only used if no design is
  given in \code{mbo} function.  Default is 20.}

  \item{init.design.fun}{[\code{function}]\cr Function from
  package lhs for the sequential design.  Possible are:
  \code{maximinLHS}, \code{randomLHS}, \code{geneticLHS},
  \code{improvedLHS}, \code{optAugmentLHS},
  \code{optimumLHS}.  Only used if no design is given in
  \code{mbo} function.  Default is \code{randomLHS}.}

  \item{init.design.args}{[\code{list}]\cr List of further
  arguments passed to \code{init.design.fun}.  Only used if
  no design is given in \code{mbo} function.  Default is
  empty list.}

  \item{iters}{[\code{integer(1)}]\cr Number of sequential
  optimization steps.  Default is 10.}

  \item{propose.points}{[\code{integer(1)}]\cr Number of
  proposed points after optimizing the surrogate model with
  \code{infill.opt}.  Default is 1.}

  \item{infill.crit}{[\code{character(1)}]\cr How should
  infill points be rated. Possible parameter values are:
  \dQuote{mean}: Mean response.  \dQuote{ei}: Expected
  improvement.}

  \item{infill.crit.lcb.lambda}{[\code{numeric(1)}]\cr
  Lambda parameter for lower confidence bound infill
  criterion.  Only used if \code{infillcrit="lcb"}, ignored
  otherwise.  Deafult is 1.}

  \item{infill.opt}{[\code{character(1)}]\cr How should
  SINGLE points be proposed by using the surrogate model.
  Possible are: \dQuote{random}: Use a large random latin
  hypercube design of points and evaluate the surrogate
  model at each.  \dQuote{cmaes}: Use CMAES to optimize
  mean prediction value.  \dQuote{ei}: Use expected
  improvement.  Default is \dQuote{random}.}

  \item{infill.opt.restarts}{[\code{integer(1)}]\cr Number
  of independent restarts for optimizer of infill
  criterion.  The first initial / start point for the
  optimizer is always the currently best point in the
  design of already visited points. Subsequent restarts are
  started at random points.  Default is 1.}

  \item{infill.opt.random.maxit}{[\code{integer(1)}]\cr For
  \code{infill.opt = "random"}: Number of iteration to
  shrink local focus.  Default is 5.}

  \item{infill.opt.random.points}{[\code{integer(1)}]\cr
  For \code{infill.opt = "random"}: Number of points in
  random search optimizer.  Default is 10000.}

  \item{infill.opt.cmaes.control}{[\code{list}]\cr For
  \code{infill.opt = "cmaes"}: Control argument for cmaes
  optimizer.  Default is empty list.}

  \item{infill.opt.es.maxit}{[\code{integer(1)}]\cr For
  \code{infill.opt = "es"}: Number of iterations /
  generations of ES.  Default is 500.}

  \item{infill.opt.es.mu}{[\code{integer(1)}]\cr For
  \code{infill.opt = "es"}: Population size of ES.  Default
  is 10.}

  \item{infill.opt.es.eta}{[\code{numeric(1)}]\cr For
  \code{infill.opt = "es"}: Distance parameter mutation
  distribution, see \code{\link[emoa]{sbx_operator}}.
  Default is ???.}

  \item{infill.opt.es.p}{[\code{numeric(1)}]\cr For
  \code{infill.opt = "es"}: Probability of 1-point
  crossover, see \code{\link[emoa]{sbx_operator}}.  Default
  is ???.}

  \item{feature.impute}{[\code{character(1)}]\cr Method
  used for imputing features, which can will be necessary
  for dependent parameters.  Possible values are:
  \dQuote{up}: Numeric vars are imputed with with 2 * upper
  bound.  \dQuote{median}: Imputes NAs with median and add
  logical is.na variable.}

  \item{multipoint.method}{[\code{character(1)}]\cr Method
  used for proposal of multiple infill points, for parallel
  batch evaluation.  Possible values are: \dQuote{lcb}:
  Proposes points by optimizing the lower confidence bound
  \dQuote{lcb} criterion, \code{propose.points} times. Each
  lambda value for \dQuote{lcb} is drawn randomly from an
  exp(1)-distribution, so do not define
  \code{infill.opt.lcb.lambda}.  The optimizer for each
  proposal is configured in the same way as for the single
  point case, i.e., by specifying \code{infill.opt} and
  related stuff.  Default is \code{lcb} \dQuote{multicrit}:
  Proposes points by evolutionary multicriteria
  optimization.  The EA is a (mu+1) type of algorithm and
  runs for \code{multipoint.multicrit.maxit} generations.
  The population size is set to \code{propose.points}.  The
  selection criterion is
  \code{multipoint.multicrit.selection}.  Default is
  \code{lcb}}

  \item{multipoint.multicrit.objective}{[\code{character(1)}]\cr
  Variants / objectives which are optimized in multicrit
  approach.  Possible are: \dQuote{ei.dist},
  \dQuote{mean.se}, \dQuote{mean.se.dist}.  Default is
  \dQuote{ei.dist}.}

  \item{multipoint.multicrit.dist}{[\code{character(1)}]\cr
  Distance function used in multicrit EA.  Possible are:
  \dQuote{nearest.neigbor}, \dQuote{nearest.better}.
  Default is \dQuote{nearest.better}.}

  \item{multipoint.multicrit.selection}{[\code{character(1)}]\cr
  Method used for selecting 1 element for removal from the
  population in each iteration of the multicrit EA.
  Possible values are: \dQuote{hypervolume}: Non-dominated
  sorting + hypervolume contribution.
  \dQuote{crowdingdist}: Non-dominated sorting + crowding
  distance based ranking.  \dQuote{first}: Non-dominated
  sorting + first objective of
  \code{multipoint.multicrit.objective} as criterion.
  \dQuote{last}: Non-dominated sorting + last objective of
  \code{multipoint.multicrit.objective} as criterion.
  Default is \code{hypervolume}}

  \item{multipoint.multicrit.maxit}{[\code{character(1)}]\cr
  Number of generations for multicrit EA.  Default is 100.}

  \item{final.method}{[\code{character(1)}]\cr How should
  the final point be proposed. Possible are:
  \dQuote{best.true.y}: Return best point ever visited
  according to true value of target function. Can be bad if
  target function is noisy.  \dQuote{last.proposed}: Return
  the last point proposed by the model.
  \dQuote{best.predicted}: Use the final model to predict
  all points ever visited and use the best one. This might
  average-out noisy function values.  Default is:
  \dQuote{best.true.y}.}

  \item{final.evals}{[\code{integer(1)}]\cr How many target
  function evals should be done at final point to reduce
  noise?  Default is 0.}

  \item{y.name}{[\code{character(1)}]\cr Name of y-column
  for target values in optimization path.  Default is
  \dQuote{y}.}

  \item{impute}{[\code{function(x, y, opt.path)}]\cr
  Function that determines the return value in case the
  original fitness functions fails (for whatever reason)
  and because of this failure returns a NA, NaN, Inf.
  \code{x} is the current x-value, \code{y} the current
  (infeasible) y-value and \code{opt.path} the current
  optimization path.  Default is to stop with an error.}

  \item{impute.errors}{[\code{logical(1)}]\cr Should
  fitness function call be wrapped in a \code{try} and the
  same imputation be used as in \code{impute}?  Default is
  \code{FALSE}.}

  \item{suppress.eval.errors}{[\code{logical(1)}]\cr Should
  reporting of error messages during target function
  evaluations be suppressed?  Only used if
  \code{impute.errors} is \code{TRUE}.  Default is
  \code{TRUE}.}

  \item{save.model.at}{[\code{integer}]\cr Sequential
  optimization iterations when the model should be saved.
  Iteration 0 is the model fit for the initial design.
  Default is \code{iters}.}

  \item{resample.at}{[\code{integer}]\cr At which
  iterations should the model be resampled and assessed?
  Iteration 0 does some resampling on the initial design.
  Default is none.}

  \item{resample.desc}{[\code{\link[mlr]{ResampleDesc}}]\cr
  How should be model be resampled?  Default is 10-fold
  CV.}

  \item{resample.measures}{[list of
  \code{\link[mlr]{Measure}}]\cr Performance measures to
  assess model with during resampling.  Default is
  \code{\link[mlr]{mse}}.}

  \item{on.learner.error}{[\code{character(1)}]\cr See
  [\code{\link[mlr]{configureMlr}}].  Default is
  \dQuote{stop}.}

  \item{show.learner.output}{[\code{logical(1)}]\cr See
  [\code{\link[mlr]{configureMlr}}].  Default is
  \code{FALSE}.}
}
\value{
  [\code{\link{MBOControl}}].
}
\description{
  Creates a control object for MBO optimization.
}


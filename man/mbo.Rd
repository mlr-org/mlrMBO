\name{mbo}
\alias{MBOResult}
\alias{mbo}
\title{Optimizes a function with sequential model based optimization.}
\usage{
mbo(fun, par.set, design = NULL, learner, control, show.info = TRUE, ...)
}
\arguments{
  \item{fun}{[\code{function(x, ...)}]\cr Fitness function
  to minimize. The first argument has to be a list of
  values.  The function has to return a single numerical
  value.}

  \item{par.set}{[\code{\link[ParamHelpers]{ParamSet}}]\cr
  Collection of parameters and their constraints for
  optimization.}

  \item{design}{[\code{data.frame} | NULL]\cr Initial
  design as data frame.  If the parameters have
  corresponding trafo functions, the design must not be
  transformed before it is passed!  If \code{NULL}, one is
  constructed from the settings in \code{control}.}

  \item{learner}{[\code{\link[mlr]{Learner}}]\cr Regression
  learner to model \code{fun}.}

  \item{control}{[\code{\link{MBOControl}}]\cr Control
  object for mbo.}

  \item{show.info}{[\code{logical(1)}]\cr Verbose output on
  console?  Default is \code{TRUE}.}

  \item{...}{[any]\cr Further arguments passed to fitness
  function.}
}
\value{
[\code{list}]: \item{x [\code{list}]}{Named list of
proposed optimal parameters.} \item{y
[\code{numeric(1)}]}{Value of fitness function at \code{x},
either from evals during optimization or from requested
final evaluations, if those were greater than 0.}
\item{opt.path
[\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
\item{models [List of
\code{\link[mlr]{WrappedModel}}]}{List of saved regression
models.} \item{multipoint.lcb.lambdas [\code{matrix(iters,
proposed.points)}]}{Sampled lambda values for multipoint
lcb method.}
}
\description{
Optimizes a function with sequential model based
optimization.
}


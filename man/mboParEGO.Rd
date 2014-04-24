\name{mboParEGO}
\alias{mboParEGO}
\title{Optimizes a multicrit optimization problem  with sequential model based
 optimization using the parEGO algorithm}
\usage{
mboParEGO(fun, par.set, design = NULL, learner, control, show.info = TRUE,
  more.args = list())
}
\arguments{
  \item{fun}{[\code{function(x, ...)}]\cr Fitness functions
  to minimize. The first argument has to be a list of
  values.  The function has to return a numerical vector of
  the length defined via the parameter number.of.targets in
  \code{control}.}

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

  \item{more.args}{[any]\cr Further arguments passed to
  fitness function.}
}
\value{
[\code{list}]: \item{pareto.front [\code{matrix}]}{Pareto
Front of all evaluated points.} \item{opt.path
[\code{\link[ParamHelpers]{OptPath}}]}{Optimization path.}
}
\description{
Optimizes a multicrit optimization problem with sequential
model based optimization using the parEGO algorithm
}


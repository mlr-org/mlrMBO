\name{autoplot.MBOExampleRun}
\alias{autoplot.MBOExampleRun}
\title{Plot example run, either in 1D or 2D.}
\usage{
  autoplot.MBOExampleRun(x, iters, pause = TRUE,
    densregion = TRUE, se.factor1 = 1, se.factor2 = 2,
    xlim, ylim, point.size = 3, trafo = NULL, ...)
}
\arguments{
  \item{x}{[\code{function}]\cr Objective function.}

  \item{...}{[\code{\link[mlr]{Learner}}]\cr Surrogate
  model used for the optimization of \code{fun}.  Default
  is mlr learner \dQuote{regr.km}, which is kriging from
  package DiceKriging. \code{nugget.estim} is set to
  \code{TRUE} depending on whether we have noisy
  observations or not.}

  \item{iters}{[\code{integer}]\cr Selected iterations of
  \code{x} to display.  Default is all iterations.}

  \item{pause}{[\code{logical(1)}]\cr Pause after each
  iteration?  Default is \code{TRUE}.}

  \item{densregion}{[\code{logical(1)}]\cr Should the
  background be shaded by the density of the posterior
  distribution?  Looks nice, but is currently pretty slow.
  You might wnat to disable this if you want to do stuff
  more interactively.  Default is \code{TRUE}.}

  \item{se.factor1}{[\code{numeric(1)}]\cr If the model
  provides local standard error estimation, in addition to
  the mean respone \code{yhat(x) +- se.factor1 * se(x)} is
  plotted above and below.  Default is 1.}

  \item{se.factor2}{[\code{numeric(1)}]\cr If the model
  provides local standard error estimation and
  \code{densregion=TRUE}, the background is shaded in the
  area \code{yhat(x) +- se.factor2 * se(x)}.  Default is
  2.}

  \item{xlim}{[\code{numeric(2)}]\cr For 1D: \code{xlim}
  parameter for first and second plot.  Default is range of
  x-values evaluated in run object \code{x}.}

  \item{ylim}{[\code{numeric(2)}]\cr For 1D: \code{ylim}
  parameter for first plot, for the second plot \code{ylim}
  is always set automatically, depending on the range of
  the evaluated infill criterion.  Default for the first
  plot is a heuristic to have the true function and
  \code{yhat(x) +- se.factor2 * se(x)} both in the plot.
  Note that this heuristic might change the \code{ylim}
  setting between plot iterations.}

  \item{...}{[\code{list}]\cr Further parameters.}
}
\value{
  Nothing.
}
\description{
  The plot will show the following elements per iteration:
  - The true objective function (solid line). - The
  surrogate approximation, represented by its mean
  response. - Surrogate mean +- 1 standard deviation, from
  local uncertainty. - Infill criterion.
}
\details{
  In both plots the following elements are present -
  Initial design points - Points from previous sequentail
  iteraions - Proposed point in current iteration.
}


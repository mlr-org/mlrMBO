##
## nsga2.R - Interface to nsga2.c
##
## Authors:
##  Heike Trautmann  <trautmann@statistik.uni-dortmund.de>
##  Detlef Steuer    <detlef.steuer@hsu-hamburg.de>
##  Olaf Mersmann    <olafm@statistik.uni-dortmund.de>
##

#' @useDynLib mlrMBO do_nsga2_vectorized


# Bernd Bischl:
# This is currently a straight-forward copy from
# http://cran.r-project.org/web/packages/mco/index.html
# Version 1.0.12
# I adapted the code in the following way:
# - only touched "evaluate_pop" in C code
# - the objective function is expected to do vectorized eval of the whole population
#   to minimize overhead (we use this to eval via predict / infill crits)
# - disabled constraint handling (dont need it now and was lazy)


nsga2_vectorized <- function(fn, idim, odim, ...,
                  constraints=NULL, cdim=0,
                  lower.bounds=rep(-Inf, idim),
                  upper.bounds=rep(Inf, idim),
                  popsize=100, generations=100,
                  cprob=0.7, cdist=5,
                  mprob=0.2, mdist=10) {

  if (!is.null(constraints))
    stop("Constraint handling is disabled in this hacked version of nsga2!")

  ff <- function(x)
    fn(x, ...)
  cf <- function(x)
    constraints(x, ...)

  ## Make sure popsize is a multiple of 4
  if (popsize %% 4 != 0)
    stop("Population size must be a multiple of 4")

  ## Lag generations:
  ## C source expects each element of generations to be the number of
  ## generations to go forward before saving the next result set. This is
  ## unintuitive to specify, so we compute the lagged differences here.
  if (length(generations) > 1)
    generations <- c(generations[1], diff(generations))

  if (!all(generations > 0))
    stop("Cannot go back in time! Your generations argument must be sorted!")

  ## Set cdim = 0 if no cfn was given:
  if (is.null(constraints)) cdim <- 0

  res <- .Call("do_nsga2_vectorized",
               ff, cf, sys.frame(),
               as.integer(odim),
               as.integer(cdim),
               as.integer(idim),
               lower.bounds, upper.bounds,
               as.integer(popsize), as.integer(generations),
               cprob, as.integer(cdist),
               mprob, as.integer(mdist))
  if (1 == length(res)) {
    res <- res[[1]]
    names(res) <- c("par", "value", "pareto.optimal")
    class(res) <- c("nsga2", "mco")
  } else {
    for (i in 1:length(res)) {
      names(res[[i]]) <- c("par", "value", "pareto.optimal")
      class(res[[i]]) <- c("nsga2", "mco")
    }
    class(res) <- "nsga2.collection"
  }
  return (res)
}


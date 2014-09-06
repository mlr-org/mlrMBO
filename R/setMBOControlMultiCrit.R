#' Extends mbo control object with multi-criteria specific options.
#'
#' @template arg_control
#' @param method [\code{character(1)}]\cr
#'   Which multicrit method should be used? At the moment only parego is
#'   supported, which is also the default.
#' @param parego.s [\code{integer(1)}]\cr
#'   Parameter of parego - controls the number of weighting vectors. The default
#'   depends on \code{number.of.targets} and leads to 100000 different possible
#'   weight vectors. The defaults for (2, 3, 4, 5, 6) dimensions are (100000,
#'   450, 75, 37, 23) and 10 for higher dimensions.
#' @param parego.rho [\code{numeric(1)}]\cr
#'   Parameter of parego - factor for Tchebycheff function. Default 0.05 as
#'   suggested in parego paper.
#' @param parego.sample.more.weights [\code{integer(1)}]\cr
#'   In each iteration \code{parego.sample.more.weights} * \code{propose.points}
#'   are sampled and the weights with maximum distance to each other are chosen.
#'   Default is 1, if only 1 point is proposed each iteration, otherwise 5.
#' @param parego.use.margin.points [\code{logical}]\cr
#'   For each target function: Should the weight vector (0, ..., 0, 1, 0, ..., 0),
#'   i.e. the weight vector with only 0 and a single 1 at the i.th position for
#'   the i.th target function, be drawn with probability 1? Number of TRUE entries
#'   must be less or equal to \code{propose.points}
#'   Default is not to do this.
#' @param parego.normalize [\code{character}] \cr
#'   Normalization to use. Either map the whole image space to [0, 1] (\code{standard}, the default)
#'   or just the paretofront (\code{front}).
#' @return [\code{\link{MBOControl}}].
#' @note See the other setMBOControl... functions and \code{makeMBOControl} for referenced arguments.
#' @seealso makeMBOControl
#' @export
setMBOControlMultiCrit = function(control,
  method = "parego",
  parego.s, parego.rho = 0.05,
  parego.use.margin.points = rep(FALSE, control$number.of.targets),
  parego.sample.more.weights = 5L,
  parego.normalize = "standard",
  mspot.ref.point = rep(11, control$number.of.targets)) {

  assertClass(control, "MBOControl")
  assertChoice(method, choices = c("parego", "mspot", "sms"))

  number.of.targets = control$number.of.targets
  propose.points = control$propose.points

  # ParEGO:
  if (missing(parego.s))
    parego.s = switch(min(control$number.of.targets, 7L),
      1L,
      100000L,
      450L,
      75L,
      37L,
      23L,
      10L)
  parego.s = asInt(parego.s)
  assertInt(parego.s, na.ok = FALSE, lower = 1)

  assertNumber(parego.rho, na.ok = FALSE, lower = 0, upper = 1)

  if (control$propose.points == 1L)
    parego.sample.more.weights = 1L
  parego.sample.more.weights = asInt(parego.sample.more.weights)
  assertInt(parego.sample.more.weights, na.ok = FALSE, lower = 1)


  assertLogical(parego.use.margin.points, len = number.of.targets, any.missing = FALSE)

  # some checks we ony want to do if we're really doing parego
  if (control$number.of.targets != 1L) {
    if (sum(parego.use.margin.points) > propose.points)
      stopf("Can't use %s margin points when only proposing %s points each iteration.",
        sum(parego.use.margin.points), propose.points)

    number.of.weights = choose(parego.s + control$number.of.targets - 1,
      control$number.of.targets - 1)
    if (parego.sample.more.weights * propose.points > number.of.weights)
      stop("Trying to sample more weights than exists. Increase parego.s or decrease number of weights.")
  }

  assertChoice(parego.normalize, choices = c("standard", "front"))

  # mspot
  assertNumeric(mspot.ref.point, any.missing = FALSE, finite = TRUE, len = control$number.of.targets)


  # extend control object
  control$multicrit.method = method
  control$parego.s = parego.s
  control$parego.rho = parego.rho
  control$parego.use.margin.points = parego.use.margin.points
  control$parego.sample.more.weights = parego.sample.more.weights
  control$parego.normalize = parego.normalize
  control$mspot.ref.point = mspot.ref.point

  return(control)
}


#FIXME: briefly explain multipoint proposal for all thre methods

#' @title Set multi-criteria options.
#' @description
#' Extends MBO control object with multi-criteria specific options.
#'
#' @template arg_control
#' @param method [\code{character(1)}]\cr
#'   Which multicrit method should be used?
#'   \dQuote{parego}: The ParEGO algorithm.
#'   \dQuote{dib}: Direct indicator-based method. Subsumes SMS-EGO and epsilon-EGO.
#'   \dQuote{mspot}: Directly optimizes multcrit problem where we substitute the true
#'     objectives with model-based infill crits via an EMOA.
#'   \dQuote{epic}: Estimates Pareto Set by classification and proposes
#'     points that are non-dominated with probability p_next.
#'     Please note: epic needs a classification learner!
#'   All methods can also propose multiple points in parallel.
#'   Default is \dQuote{dib}.
#' @param ref.point.method [\code{character(1)}] \cr
#'   Method for the determination of the reference point used for S-metric.
#'   Currently used for \dQuote{mspot} and \dQuote{dib} with indicator \dQuote{sms}.
#'   Possible Values are:
#'   \dQuote{all}: In each dimension: maximum of all points + \code{ref.point.offset}.
#'   \dQuote{front}: In each dimension: maximum of all non-dominated points + \code{ref.point.offset}
#'   \dQuote{const}: Constant value, see \code{ref.point.val}.
#'   Default is \dQuote{all}.
#' @param ref.point.offset [\code{numeric(1)}]\cr
#'   See \code{ref.point.method}, default is 1.
#' @param ref.point.val [\code{numeric}]\cr
#'   Constant value of reference point for hypervolume calculation.
#'   Used if \code{ref.point.method = "const"}. Has to be specified in this case.
#' @param parego.s [\code{integer(1)}]\cr
#'   Parameter of parego - controls the number of weighting vectors. The default
#'   depends on \code{n.objectives} and leads to ca. 100000 different possible
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
#' @param dib.indicator [\code{character(1)}]\cr
#'   Either \dQuote{sms} (SMS-EGO like algorithm) or \dQuote{eps} (epsilon-EGO like algorithm).
#'   Default is \dQuote{sms}.
#' @param dib.sms.eps [\code{numeric(1)} | \code{NULL}]\cr
#'   Epsilon for epsilon-dominance for \code{dib.indicator = "sms"}.
#'   Default is \code{NULL}, in this case it is adaptively set.
#' @param mspot.select.crit [\code{character(1)}]\cr
#'   Which infill.crit to use in the candidate selection. After the NSGA2
#'   proposed a set of candidates, \dQuote{propose.points} are selected via
#'   the hypervoume contribution of this infill.crit.
#'   Possible values are \dQuote{mean} and \dQuote{cb}, default ist \dQuote{mean}
#' @param epic.design.size [\code{integer(1)}]\cr
#'   Size of the initial design of epic. Please do note the special behaviour of epic:
#'   The \dQuote{design} parameter does not equal the initial design, but a sampled
#'   decision space. Each evaluated point must be one from this sampled decision space!
#'   Hence, the initial design is a subset of the \dQuote{design}, and this parameter
#'   controls the size of the initial design. Default is 4 * d where d is the
#'   dimension of the parameter set.
#' @param epic.p.nond [\code{numeric(1)}]\cr
#'   Minimum probability of non-domination for the next proposed point.
#'   Must be in [0, 1], default is 0.6
#' @param epic.p.next [\code{numeric}]\cr
#'   Vector of desired probabilites for the next proposed points.
#'   Must be of length \dQuote{propose.points}. Default is rep(0.8, \dQuote{propose.points}).
#'   @param epo.p.type [\code{numeric}]\cr
#'     TODO
#'   @param epo.p.method [\code{numeric}]\cr
#'     TODO
#'   @param epo.p.scal [\code{numeric}]\cr
#'     TODO
#' @return [\code{\link{MBOControl}}].
#'
#' @references
#' For more information on the implemented multi-criteria procedures the following
#' sources might be helpful:
#' Horn, Daniel; Wagner, Tobias; Biermann, Dirk; Weihs, Claus; Bischl, Bernd;
#' Model-Based Multi-objective Optimization: Taxonomy, Multi-Point Proposal, Toolbox and Benchmark
#' Evolutionary Multi-Criterion Optimization ,64-78, 2015, Springer
#' 
#' Knowles, J.: ParEGO: A hybrid algorithm with on-line landscape
#' approximation for expensive multiobjective optimization problems. IEEE
#' Transactions on Evolutionary Computation, 10 (2006) 1, pp. 50-66
#'
#' Wagner, T.; Emmerich, M.; Deutz, A.; Ponweiser, W.: On Expected-
#' Improvement Criteria for Model-Based Multi-Objective Optimization.
#' In: Proc. 11th Int. Conf. Parallel Problem Solving From Nature (PPSN
#' XI) - Part I, Krakow, Poland, Schaefer, R.; Cotta, C.; Kolodziej, J.;
#' Rudolph, G. (eds.), no. 6238 in Lecture Notes in Computer Science,
#' Springer, Berlin, 2010, ISBN 978-3-642-15843-8, pp. 718-727, doi:10.
#' 1007/978-3-642-15844-5 72
#'
#' Wagner, T.: Planning and Multi-Objective Optimization of Manufacturing
#' Processes by Means of Empirical Surrogate Models.
#' No. 71 in Schriftenreihe des ISF, Vulkan Verlag, Essen, 2013, ISBN
#' 978-3-8027-8775-1
#'
#' Zaefferer, M.; Bartz-Beielstein, T.; Naujoks, B.; Wagner, T.;
#' Emmerich, M.: A Case Study on Multi-Criteria Optimization of
#' an Event Detection Software under Limited Budgets. In: Proc.
#' 7th International. Conf. Evolutionary Multi-Criterion Optimization (EMO
#' 2013), March 19-22, Sheffield, UK, R. Purshouse; P. J. Fleming;
#' C. M. Fonseca; S. Greco; J. Shaw, eds., 2013, vol. 7811 of Lecture
#' Notes in Computer Science, ISBN 978-3-642-37139-4, pp. 756{770,
#' doi:10.1007/978-3-642-37140-0 56}
#'
#' Jeong, S.; Obayashi, S.: Efficient global optimization (EGO) for multiobjective
#' problem and data mining. In: Proc. IEEE Congress on
#' Evolutionary Computation (CEC 2005), Edinburgh, UK, Corne, D.;
#' et al. (eds.), IEEE, 2005, ISBN 0-7803-9363-5, pp. 2138-2145
#'
#' @family MBOControl
#' @export
setMBOControlMultiCrit = function(control,
  method = NULL,
  ref.point.method = NULL,
  ref.point.offset = NULL,
  ref.point.val = NULL,
  parego.s = NULL,
  parego.rho = NULL,
  parego.use.margin.points = NULL,
  parego.sample.more.weights = NULL,
  parego.normalize = NULL,
  dib.indicator = NULL,
  dib.sms.eps = NULL,
  mspot.select.crit = NULL,
  epic.design.size = NULL,
  epic.p.nond = NULL,
  epic.p.next = NULL,
  epo.p.method,
  epo.p.scal,
  epo.type) {

  assertClass(control, "MBOControl")
  n.objectives = control$n.objectives
  propose.points = control$propose.points
  if (n.objectives == 1L)
    stop("You are setting multicrit options, but have only 1 objective!")
  requirePackages(c("mco", "emoa"), why = "multicrit optimization")

  control$multicrit.method = coalesce(method, control$multicrit.method, "dib")
  assertChoice(control$multicrit.method, choices =
      c("parego", "mspot", "dib", "epic", "epo", "epo"))

  # Reference Point
  control$multicrit.ref.point.method = coalesce(ref.point.method, control$multicrit.ref.point.method, "all")
  assertChoice(control$multicrit.ref.point.method, choices = c("all", "front", "const"))

  control$multicrit.ref.point.offset = coalesce(ref.point.offset, control$multicrit.ref.point.offset, 1)
  assertNumber(control$multicrit.ref.point.offset, lower = 0, finite = TRUE)

  if (control$multicrit.ref.point.method == "const") {
    if (is.null(ref.point.val) & is.null(control$multicrit.ref.point.val))
      stopf("Constant reference point has to be specified.")
    else
      control$multicrit.ref.point.val = coalesce(ref.point.val, control$multicrit.ref.point.val)
      assertNumeric(control$multicrit.ref.point.val, any.missing = FALSE, finite = TRUE, len = n.objectives)
  }

  if (control$multicrit.method == "parego") {
    if (missing(parego.s)) {
      parego.s = switch(min(n.objectives, 7L),
        1L,
        100000L,
        450L,
        75L,
        37L,
        23L,
        10L)
    } else {
      parego.s = asInt(parego.s, na.ok = FALSE, lower = 1)
    }
    control$multicrit.parego.s = coalesce(parego.s, control$multicrit.parego.s)

    control$multicrit.parego.rho = coalesce(parego.rho, control$multicrit.parego.rho, 0.05)
    assertNumber(control$multicrit.parego.rho, na.ok = FALSE, lower = 0)

    if (propose.points == 1L) {
      parego.sample.more.weights = 1L
    } else if (!is.null(parego.sample.more.weights)) {
      parego.sample.more.weights = asInt(parego.sample.more.weights)
    }
    control$multicrit.parego.sample.more.weights = coalesce(parego.sample.more.weights, control$multicrit.parego.sample.more.weights, 5L)
    assertInt(control$multicrit.parego.sample.more.weights, na.ok = FALSE, lower = 1)

    control$multicrit.parego.use.margin.points = coalesce(parego.use.margin.points, control$multicrit.parego.use.margin.points, rep(FALSE, control$n.objectives))
    assertLogical(control$multicrit.parego.use.margin.points, len = n.objectives, any.missing = FALSE)

    if (sum(control$multicrit.parego.use.margin.points) > propose.points)
      stopf("Can't use %s margin points when only proposing %s points each iteration.",
        sum(control$multicrit.parego.use.margin.points), propose.points)

    number.of.weights = choose(parego.s + n.objectives - 1L, n.objectives - 1L)
    if (control$multicrit.parego.sample.more.weights * propose.points > number.of.weights)
      stop("Trying to sample more weights than exists. Increase parego.s or decrease number of weights.")

    control$multicrit.parego.normalize = coalesce(parego.normalize, control$multicrit.parego.normalize, "standard")
    assertChoice(control$multicrit.parego.normalize, choices = c("standard", "front"))
  }

  # DIB
  if (control$multicrit.method == "dib") {
    control$multicrit.dib.indicator = coalesce(dib.indicator, control$multicrit.dib.indicator, "sms")
    assertChoice(control$multicrit.dib.indicator, c("sms", "eps"))

    if (!is.null(dib.sms.eps)) {
      assertNumber(dib.sms.eps, lower = 0, finite = TRUE)
      control$multicrit.dib.sms.eps = dib.sms.eps
    }
  }

  control$mspot.select.crit = coalesce(mspot.select.crit, control$mspot.select.crit, "mean")
  assertChoice(control$mspot.select.crit, choices = c("mean", "cb"))

  
  control$multicrit.epic.design.size = coalesce(epic.design.size, control$multicrit.epic.design.size)
  if (!is.null(control$epic.design.size))
    control$multicrit.epic.design.size = asCount(control$multicrit.epic.design.size, positive = TRUE)
  
  control$multicrit.epic.p.nond = coalesce(epic.p.nond, control$multicrit.epic.p.nond, 0.6)
  assertNumber(control$multicrit.epic.p.nond, lower = 0, upper = 1)
  

  control$multicrit.epic.p.next = coalesce(epic.p.next, control$multicrit.epic.p.next,
    rep(0.8, control$propose.points))
  assertNumeric(control$multicrit.epic.p.next, lower = 0, upper = 1, any.missing = FALSE,
    len = control$propose.points)
  
  control$multicrit.epo.type = coalesce(epo.type, control$multicrit.epo.type, "classif")
  assertChoice(control$multicrit.epo.type, choices = c("classif", "regr"))
  
  control$multicrit.epo.p.method = coalesce(epo.p.method, control$multicrit.epo.p.method, "const05")
  assertChoice(control$multicrit.epo.p.method, choices =
      c("const05", "trunc.norm", "sqrt", "linear", "runif"))
  
  control$multicrit.epo.p.scal = coalesce(epo.p.scal, control$multicrit.epo.p.scal, "tscheb")
  assertChoice(control$multicrit.epo.p.scal, choices = c("tscheb", "cut"))
  
  return(control)
}

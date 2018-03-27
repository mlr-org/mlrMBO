#' @title Create an infill criterion.
#'
#' @description The infill criterion guides the model based search process.
#' The most prominent infill criteria, e.g., expected improvement, lower
#' confidence bound and others, are already implemented in mlrMBO. Moreover,
#' the package allows for the creation of custom infill criteria.
#'
#' @param fun [\code{function(points, models, control, par.set, design, iter)}]\cr
#'   A function which expects the following parameters in exactly this order
#'   and return a numeric vector of criteria values at the points:
#'   \describe{
#'     \item{points [\code{data.frame}]}{n points where to evaluate.}
#'     \item{models [\code{\link[mlr]{WrappedModel}} | \code{list}]}{Model(s) fitted on design.}
#'     \item{control [\code{MBOControl}]}{Control object.}
#'     \item{par.set [\code{ParamSet}]}{Parameter set.}
#'     \item{design [\code{data.frame}]}{Design of already visited points.}
#'     \item{iter [\code{integer(1)}]}{Current iteration.}
#'     \item{progress [\code{numeric{1}}]}{A value between 0 and 1 indicating the progress of the optimization.}
#'     \item{attributes [\code{logical{1}}]}{Are there attributes appended to the return
#'      value that should be added to the \code{OptPath}?}
#'   }
#'  Important: Internally, this function will be minimized. So the proposals will be where this function is low.
#' @param name [\code{character(1)}]\cr
#'   Full name of the criterion.
#' @param id [\code{character(1)}]\cr
#'   Short name of the criterion.
#'   Used internally and in plots.
#' @param opt.direction [\code{character(1)}]\cr
#'   Only for visualization: Shall this criterion be plotted as if it were to be minimized (\code{minimize}), maximized (\code{maximize}) or is the direction the same as for the objective function (\code{objective})?
#'   Default is \code{minimize}.
#' @param components [\code{character}]\cr
#'   Infill criteria may not return proposed point(s) only. Additional
#'   information can be returned by appending a named \code{list} \dQuote{crit.components}
#'   to the returned value as an attribute.
#'   The \code{components} argument takes a character vector of the names of the
#'   meta information, i.e., the names of the named \dQuote{crit.components} list.
#'   Default is the empty character vector.
#' @param params [\code{list}]\cr
#'   Named list of parameters for the infill criterion. There values may be used
#'   by \pkg{mlrMBO} internally.
#'   Default is the empty list.
#' @param requires.se [\code{logical(1)}]\cr
#'   Does the infill criterion require the regression learner to provide a standard
#'   error estimation?
#'   Default is \code{FALSE}.
#' @return [\code{\link{MBOInfillCrit}}]
#' @rdname MBOInfillCrit
#' @aliases MBOInfillCrit
#' @export
makeMBOInfillCrit = function(fun, name, id,
  opt.direction = "minimize", components = character(0L), params = list(),
  requires.se = FALSE) {
  assertFunction(
    fun,
    args = c("points", "models", "control", "par.set", "designs", "iter", "progress", "attributes"),
    ordered = TRUE)

  assertString(name)
  assertString(id)
  assertChoice(opt.direction, c("minimize", "maximize", "objective"))
  assertCharacter(components, unique = TRUE)
  assertList(params)
  assertFlag(requires.se)

  ic = makeS3Obj(c(paste0("InfillCrit", toupper(id)), "MBOInfillCrit"),
    fun = fun,
    name = name,
    id = id,
    opt.direction = opt.direction,
    components = components,
    params = params,
    requires.se = requires.se
  )
  return(ic)
}

#' @export
print.MBOInfillCrit = function(x, ...) {
  components = getMBOInfillCritComponents(x)
  params = getMBOInfillCritParams(x)
  catf("Infill criterion            : %s (%s)", getMBOInfillCritName(x),
    getMBOInfillCritId(x))
    catf("  Direction of optimization : %s", x$opt.direction)
  if (hasRequiresInfillCritStandardError(x))
    catf("  Requirement               : SE estimation")
  if (length(components) > 0)
    catf("  Components                : %s", collapse(components, sep = ", "))
  if (length(params) > 0)
    catf("  Parameters                : %s", convertToShortString(params))
}

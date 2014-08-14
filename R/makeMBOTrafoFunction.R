#' Create transformation function for MBOExampleRun.
#'
#' @param name [\code{character(1)}]\cr
#'   Name of the transformation.
#' @param fun [\code{function}]\cr
#'   R function which expects a numeric vector.
#' @return Object of type MBOTrafoFunction.
#' @seealso \link{trafos}
#' @export
makeMBOTrafoFunction = function(name, fun) {
  assertString(name, na.ok = FALSE)
  assertFunction(fun)
  fun = addClasses(fun, "MBOTrafoFunction")
  fun = setAttribute(fun, "name", name)
  return(fun)
}

#' Transformation methods.
#'
#' \itemize{
#'   \item{\bold{logTrafo}}{\cr Natural logarithm.}
#'   \item{\bold{sqrtTrafo}}{\cr Square root.}
#' }
#' If negative values occur and the trafo function can handle only positive values,
#' a shift of the form x - min(x) + 1 is performed prior to the transformation if the
#' argument \code{handle.violations} is set to \dQuote{warn} which is the default
#' value. 
#' @format None
#' @name trafos
#' @rdname trafos
NULL

#' @export
#' @rdname trafos
#' @param base [\code{numeric(1)}]\cr
#'   The base with respect to which logarithms are computed.
#'   Default is \code{10}.
trafoLog = function(base = 10) {
  assertNumber(base, na.ok = FALSE, lower = 2L)
  makeMBOTrafoFunction(
    name = "log",
    fun = function(x, handle.violations = "warn") {
      x = checkAndRepairNegativeValues(x, handle.violations)
      log(x, base = base)
    }
  )
}

#' @export
#' @rdname trafos
trafoSqrt = function() {
  makeMBOTrafoFunction(
    name = "sqrt",
    fun = function(x, handle.violations = "warn") {
      x = checkAndRepairNegativeValues(x, handle.violations)
      sqrt(x)
    }
  )
}

checkAndRepairNegativeValues = function(x, handle.violations) {
  assertChoice(handle.violations, c("warn", "error"))
  if (any(x < 0)) {
    if (handle.violations == "error")
      stopf("Negative function values occured during transformation.")
    warning("Negative function values. Shifting function to apply logarithm.")
    return(x - min(x) + 1)
  }
  return(x)
}

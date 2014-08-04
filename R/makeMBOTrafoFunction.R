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
  fun = addClasses(fun, "MBOTrafoFunction")
  fun = setAttribute(fun, "name", name)
  return(fun)
}

#' Transformation methods.
#'
#' \itemize{
#' \item{\bold{logTrafo}}{\cr Natural logarithm.}
#' \item{\bold{log10Trafo}}{\cr Logartihm to base 10.}
#' \item{\bold{sqrtTrafo}}{\cr Square root.}
#' }
#' If negative values occur and the trafo function can handle only positive values,
#' a shift of the form x - min(x) + 1 is performed prior to the transformation.
#' @format None
#' @name trafos
#' @rdname trafos
NULL

#' @export
#' @rdname trafos
trafoLog = function() {
  makeMBOTrafoFunction(
    name = "log",
    fun = function(x) {
      if (any(x < 0)) {
        warning("Negative function values. Shifting function to apply logarithm.")
        x = x - min(x) + 1
      }
      return(log(x))
    }
  )
}

#' @export
#' @rdname trafos
trafoLog10 = function() {
  makeMBOTrafoFunction(
    name = "log10",
    fun = function(x) {
      if (any(x < 0)) {
        warning("Negative function values. Shifting function to apply logarithm.")
        x = x - min(x) + 1
      }
      return(log10(x))
    }
  )
}

#' @export
#' @rdname trafos
trafoSqrt = function() {
  makeMBOTrafoFunction(
    name = "sqrt",
    fun = function(x) {
      sqrt(x)
    }
  )
}

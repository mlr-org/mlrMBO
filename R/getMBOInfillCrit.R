#' @title Get properties of MBO infill criterion.
#'
#' @description
#' Returns properties of an infill criterion, e.g., name or id.
#'
#' @param x [\code{\link{MBOInfillCrit}}]\cr
#'   Infill criterion.
#' @param par.name [\code{character(1)}]\cr
#'   Parameter name.
#' @name getMBOInfillCrit
#' @rdname getMBOInfillCrit
NULL

#' @export
#' @rdname getMBOInfillCrit
getMBOInfillCritParams = function(x) {
  assertClass(x, "MBOInfillCrit")
  return(x$params)
}

#' @export
#' @rdname getMBOInfillCrit
getMBOInfillCritParam = function(x, par.name) {
  assertClass(x, "MBOInfillCrit")
  assertString(par.name)
  return(getMBOInfillCritParams(x)[[par.name]])
}

#' @export
#' @rdname getMBOInfillCrit
getMBOInfillCritName = function(x) {
  assertClass(x, "MBOInfillCrit")
  return(x$name)
}

#' @export
#' @rdname getMBOInfillCrit
getMBOInfillCritId = function(x) {
  assertClass(x, "MBOInfillCrit")
  return(x$id)
}

#' @export
#' @rdname getMBOInfillCrit
hasRequiresInfillCritStandardError = function(x) {
  assertClass(x, "MBOInfillCrit")
  return(x$requires.se)
}

#' @export
#' @rdname getMBOInfillCrit
getMBOInfillCritComponents = function(x) {
  assertClass(x, "MBOInfillCrit")
  return(x$components)
}

getMBOInfillCritDummyComponents = function(x) {
  assertClass(x, "MBOInfillCrit")
  ns = getMBOInfillCritComponents(x)
  as.data.frame(BBmisc::namedList(ns, NA_real_))
}

#' @export
#' @rdname getMBOInfillCrit
getMBOInfillCritDirection = function(x) {
  assertClass(x, "MBOInfillCrit")
  ifelse(x$minimize, 1, -1)
}

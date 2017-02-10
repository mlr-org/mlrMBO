#' @title Get properties of MBO infill criterion.
#'
#' @description
#' Returns properties of an infill criterion, e.g., name or id.
#'
#' @param x [\code{\link{MBOInfillCriterion}}]\cr
#'   Infill criterion.
#' @param par.name [\code{character(1)}]\cr
#'   Parameter name.
#' @name getMBOInfillCriterion
#' @rdname getMBOInfillCriterion
NULL

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCritParams = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$params)
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCritParam = function(x, par.name) {
  assertClass(x, "MBOInfillCriterion")
  assertString(par.name)
  return(getMBOInfillCritParams(x)[[par.name]])
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCritName = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$name)
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCritId = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$id)
}

#' @export
#' @rdname getMBOInfillCriterion
hasRequiresInfillCritStandardError = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$requires.se)
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCritComponents = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$components)
}

getMBOInfillCritDummyComponents = function(x) {
  assertClass(x, "MBOInfillCriterion")
  ns = getMBOInfillCritComponents(x)
  as.data.frame(BBmisc::namedList(ns, NA_real_))
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCritDirection = function(x) {
  assertClass(x, "MBOInfillCriterion")
  ifelse(x$minimize, 1, -1)
}

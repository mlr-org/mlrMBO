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
getMBOInfillCriterionParams = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$params)
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCriterionParam = function(x, par.name) {
  assertClass(x, "MBOInfillCriterion")
  assertString(par.name)
  return(getMBOInfillCriterionParams(x)[[par.name]])
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCriterionName = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$name)
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCriterionId = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$id)
}

#' @export
#' @rdname getMBOInfillCriterion
hasRequiresInfillCriterionStandardError = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$requires.se)
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCriterionComponents = function(x) {
  assertClass(x, "MBOInfillCriterion")
  return(x$components)
}

getMBOInfillCriterionDummyComponents = function(x) {
  assertClass(x, "MBOInfillCriterion")
  ns = getMBOInfillCriterionComponents(x)
  as.data.frame(BBmisc::namedList(ns, NA_real_))
}

#' @export
#' @rdname getMBOInfillCriterion
getMBOInfillCriterionDirection = function(x) {
  assertClass(x, "MBOInfillCriterion")
  ifelse(x$minimize, 1, -1)
}

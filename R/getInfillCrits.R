#' Returns the names of all currently supported infill criteria.
#'
#' @return [\code{character}]
#' @export
getInfillCrits = function() {
  c("mean", "ei", "aei", "lcb")
}


#' Returns the names of all currently supported infill criteria.
#'
#' @return [\code{character}]
#' @export
getSupportedInfillCritFunctions = function() {
  c("mean", "se", "ei", "aei", "lcb", "dib", "eps", "multiFid")
}


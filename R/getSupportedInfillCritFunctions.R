#' Returns the names of all currently supported infill criteria.
#'
#' @return [\code{character}]
#' @export
getSupportedInfillCritFunctions = function() {
  c("mean", "se", "ei", "aei", "eqi","lcb", "dib", "multiFid")
}


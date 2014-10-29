#' Returns the names of all currently supported infill criteria.
#'
#' @return [\code{character}]
#' @export
getSupportedInfillCritFunctions = function() {
  # FIXME: DH: Noisy infill crits not supported atm!
  c("mean", "se", "ei", "aei", "eqi", "lcb", "dib", "multiFid")
  #c("mean", "se", "ei","lcb", "dib", "multiFid")
}


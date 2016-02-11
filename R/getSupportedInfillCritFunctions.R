#' @title Returns the names of all currently supported infill criteria.
#'
#' @return [\code{character}]
#' @export
getSupportedInfillCritFunctions = function() {
  # FIXME: DH: Noisy infill crits not supported atm!
  c("mean", "se", "ei", "aei", "eqi", "cb", "dib")
  #c("mean", "se", "ei","cb", "dib", "multiFid")
}


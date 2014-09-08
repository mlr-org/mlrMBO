#' Get names of supported infill-criteria optimizers.
#'
#' @return [\code{character}]
#' @export
getSupportedInfillOptFunctions = function() {
  c("cmaes", "focussearch", "ea", "nsga2")
}

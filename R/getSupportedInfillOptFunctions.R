#' @title Get names of supported infill-criteria optimizers.
#' @description
#' None.
#' @return [\code{character}]
#' @export
getSupportedInfillOptFunctions = function() {
  c("cmaes", "focussearch", "ea", "nsga2")
}

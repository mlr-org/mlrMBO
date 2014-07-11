#' Get names of supported multipoint infill-criteria opimizers.
#'
#' @return [\code{character}]
#' @export
getSupportedMultipointInfillOptFunctions = function() {
  c("cl", "lcb", "multicrit")
}
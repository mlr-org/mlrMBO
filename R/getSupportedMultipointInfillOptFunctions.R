#' @title Get names of supported multi-point infill-criteria optimizers.
#'
#' @description
#' Returns all names of supported multi-point infill-criteria optimizers.
#'
#' @return [\code{character}]
#' @export
getSupportedMultipointInfillOptFunctions = function() {
  c("cl", "cb", "moimbo")
}

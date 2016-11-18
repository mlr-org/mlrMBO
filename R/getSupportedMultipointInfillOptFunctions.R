#' @title Get names of supported multipoint infill-criteria optimizers.
#'
#' @description
#' Returns all names of supported multipoint infill-criteria optimizers.
#'
#' @return [\code{character}]
#' @export
getSupportedMultipointInfillOptFunctions = function() {
  c("cl", "cb", "moimbo")
}

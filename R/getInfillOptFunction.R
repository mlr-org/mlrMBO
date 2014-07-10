# trivial dispatcher
getInfillOptFunction = function(infill.opt) {
  switch(infill.opt,
    cmaes = infillOptCMAES,
    focussearch = infillOptFocus,
    ea = infillOptEA,
    # default: try to match the fun which is given as string
    match.fun(infill.opt)
  )
}

#' Get supported infill-criteria optimizers.
#'
#' @return [\code{list}] List of supported infill optimizers.
#' @export
getSupportedInfillOptFunctions = function() {
  c("cmaes", "focussearch",  "ea")
}

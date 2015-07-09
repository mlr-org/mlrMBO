
# Sets up the correct format for trafo functions used
# by MBOExampleRun plot functions.
#
# @param n.params [\code{integer(1)}]\cr
#   Number of parameters.
# @param input.trafo [\code{list}]\cr
#   List of trafo functions provided by the user.
# @return [\code{list}]\cr
#   List of trafo functions with format that is expected by exampleRun plot functions.
buildTrafoList = function(n.params, input.trafo) {
  if (n.params == 1) {
    assertSubset(names(input.trafo), choices = c("y", "crit"))
    trafo.defaults = list("y" = NULL, "crit" = NULL)
  } else {
    assertSubset(names(input.trafo), choices = c("y", "yhat", "crit", "se"))
    trafo.defaults = list("y" = NULL, "yhat" = NULL, "crit" = NULL, "se" = NULL)
  }
  
  if (is.null(input.trafo))
    return(trafo.defaults)
  
  # if single function provided, apply it to all plots
  if (c("MBOTrafoFunction") %in% class(input.trafo)) {
    if (n.params == 1) {
      trafo = list("y" = input.trafo, "crit" = input.trafo)
    } else {
      trafo = list("y" = input.trafo, "yhat" = input.trafo, "crit" = input.trafo, "se" = input.trafo)
    }
  } else {
    # otherwise check if all elements are of an appropriate type
    lapply(input.trafo, function(t)
      if(!is.null(t)) assertClass(t, "MBOTrafoFunction")
    )
    trafo = trafo.defaults
    trafo[names(input.trafo)] = input.trafo
  }
  return(trafo)
}

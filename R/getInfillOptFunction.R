# Trivial dispatcher for infill optimizers.
#
# @note Keep in mind to update getSupportedInfillOptFunctions too,
# if a new method is implemented.
#
# @param infill.opt [\code{character(1)}]\cr
#   String key for infill optimizer.
# @return [\code{function}]
getInfillOptFunction = function(infill.opt) {
  switch(infill.opt,
    cmaes = infillOptCMAES,
    focussearch = infillOptFocus,
    ea = infillOptEA,
    nsga2 = infillOptMultiObjNSGA2,
    # default: try to match the fun which is given as string
    match.fun(infill.opt)
  )
}

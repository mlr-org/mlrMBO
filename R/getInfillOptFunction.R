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

getSupportedInfillOptFunctions = function() {
  c("cmeas", "focussearch", "ea")
}

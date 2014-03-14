# trivial dispatcher
getMultipointInfillOptFunction = function(multipoint.infill.opt) {
  switch(multipoint.infill.opt,
    cl = multipointInfillOptCL,
    lcb = multipointInfillOptLCB,
    multicrit = multipointInfillOptMulticrit,
    # default: try to match the fun which is given multipoint.as string
    match.fun(multipoint.infill.opt)
  )
}

# Trivial dispatcher for multipoint infill optimizers.
#
# @note Keep in mind to update getSupportedMultipointInfillOptFunctions too,
# if a new method is implemented.
#
# @param multipoint.infill.opt [\code{character(1)}]\cr
#   String key for multipoint infill optimizer.
# @return [\code{function}]
getMultipointInfillOptFunction = function(multipoint.infill.opt) {
  switch(multipoint.infill.opt,
    cl = multipointInfillOptCL,
    lcb = multipointInfillOptLCB,
    multicrit = multipointInfillOptMulticrit,
    # default: try to match the fun which is given multipoint.as string
    match.fun(multipoint.infill.opt)
  )
}

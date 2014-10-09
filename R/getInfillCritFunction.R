# Trivial dispatcher for infill criterions.
#
# @note Keep in mind to update getSupportedInfillCritFunctions too,
# if a new method is implemented.
#
# @param infill.crit [\code{character(1)}]\cr
#   String key for infill criterion.
# @return [\code{function}]
getInfillCritFunction = function(infill.crit) {
  switch(infill.crit,
    mean = infillCritMeanResponse,
    se = infillCritStandardError,
    ei = infillCritEI,
    aei = infillCritAEI,
    lcb = infillCritLCB,
    # akg = infillCritAKG,
     eqi = infillCritEQI,
    # mq  = infillCritMQ,
    # eipi  =  infillCritEIPI,
    dib = infillCritDIB,
    multiFid = infillCritMultiFid,
    match.fun(infill.crit)
  )
}


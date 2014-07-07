# trivial dispatcher
getInfillCritFunction = function(infill.crit) {
  switch(infill.crit,
    mean = infillCritMeanResponse,
    ei = infillCritEI,
    # aei = infillCritAEI,
    lcb = infillCritLCB,
    # akg = infillCritAKG,
    # eqi = infillCritEQI,
    # mq  = infillCritMQ,
    # eipi  =  infillCritEIPI,
    multiFid = infillCritMultiFid,
    match.fun(infill.crit)
  )
}


#FIXME: export
getInfillCrits = function() {
  c("mean", "ei", "aei", "lcb")
}

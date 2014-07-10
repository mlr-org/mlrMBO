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


#' Get supported infill-criteria.
#'
#' @return [\code{list}] List of supported infill-criteria.
#' @export
getInfillCrits = function() {
  c("mean", "ei", "aei", "lcb")
}

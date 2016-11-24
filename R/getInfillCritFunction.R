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
    mean = makeMBOInfillCriterionMeanResponse(),
    se = makeMBOInfillCriterionStandardError(),
    ei = makeMBOInfillCriterionEI(),
    aei = makeMBOInfillCriterionAEI(),
    cb = makeMBOInfillCriterionCB(),
    # akg = makeMBOInfillCriterionAKG(),
     eqi = makeMBOInfillCriterionEQI(),
    # mq  = makeMBOInfillCriterionMQ(),
    # eipi  =  makeMBOInfillCriterionEIPI(),
    dib = makeMBOInfillCriterionDIB(),
    multifid = makeMBOInfillCriterionMultiFid(),
    match.fun(infill.crit)
  )
}

# Generates the Proposal that is universally used internally
# @param control [MBOcontrol]
# @param prop.points [data.frame (nxk)]
#   columns: each parameter
#   rows: each point proposal (usually 1-row, multi-point: more rows)
# @param propose.time [numeric (n)]
#   Vector with time it took for each proposal corresponding to each row (see above)
# @param prop.type [character (n)]
#   Character telling us how this proposal was created
# @param crit.vals [matrix (nxp)]
#   Values of the infill criterion
# @param crit.components [data.frame (nxq)]
#   Components that generated the infill criterion
# @errors.model [character]
#   Error message in case something went wrong.
makeProposal = function(control, prop.points, propose.time = NULL, prop.type, crit.vals = NULL, crit.components = NULL, errors.model = NA_character_) {

  n.points = coalesce(nrow(prop.points), control$propose.points)

  if (is.null(propose.time)) {
    propose.time = rep(NA_real_, n.points)
  }


  if (is.null(crit.vals)) {
    if (control$n.objectives > 1L && control$multiobj.method == "mspot")
      # mspot is the special kid, that needs multiple crit vals
      ncol = control$n.objectives + 1
    else
      ncol = 1
    crit.vals = matrix(NA_real_, nrow = n.points, ncol = ncol)
  }

  if (is.null(crit.components) && !is.null(control$infill.crit$components)) {
    crit.components = getMBOInfillCritDummyComponents(control$infill.crit)
    # crit.components is a data.frame with one row
    # we need the dummy values for all n.points
    crit.components[seq_len(n.points), ] = crit.components[1, ]
  }

  res = list(
    prop.points = prop.points,
    propose.time = propose.time,
    prop.type = prop.type,
    crit.vals = crit.vals,
    crit.components = crit.components,
    errors.model = errors.model)
  addClasses(res, "Proposal")
}

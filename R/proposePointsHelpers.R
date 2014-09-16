# some functions call proposePointByInfillOptimization multiple times
# this helpers joins these results to a single result
joinProposedPoints = function(props) {
  list(
    prop.points = do.call(rbind, extractSubList(props, "prop.points", simplify = FALSE)),
    crit.vals = do.call(rbind, extractSubList(props, "crit.vals", simplify = FALSE)),
    errors.model = do.call(c, extractSubList(props, "errors.model", simplify = FALSE))
  )
}

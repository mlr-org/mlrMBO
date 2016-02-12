# Calculates gap between best point and global optimum.
#
# Used in the exampleRun functions and printed in plots.
#
# @param design [\code{data.frame}]\cr
#   Initial design as data frame.
# @param global.opt [\code{numeric(1)]\cr
#   Value of the known global optimum.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{numeric(1)}]
calculateGap = function(design, global.opt, control) {
  best.y = if (control$minimize) min(design[, control$y.name]) else max(design[, control$y.name])
  abs(best.y - global.opt)
}

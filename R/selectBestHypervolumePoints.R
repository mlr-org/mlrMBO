# gets a data.frame of candidate points and selects the control$prop.points best points
# concerning hypervolume
# returns the indices of the best points
selectBestHypervolumePoints = function (crit.vals, control, opt.path) {
  n = nrow(crit.vals)
  front.old = getOptPathY(opt.path)
  hypervolume.old = dominated_hypervolume(t(front.old), control$mspot.ref.point)
  hypervolume.new = sapply(1:n, function(i) dominated_hypervolume(t(rbind(front.old, crit.vals[i, ]))))
  
  hypervolume.contr = hypervolume.new - hypervolume.old
  order(hypervolume.contr)[1:control$propose.points]
}
# Determines the reference point for multicrit optimization
# Gets controlobject to determine the method and actual design
# Returns reference-point, numeric vector of length number.of.targets

getMulticritRefPoint = function (control, design) {
  
  if (control$multicrit.ref.point.method == "const")
    ref.point = control$multicrit.ref.point.val
  
  if (control$multicrit.ref.point.method == "all") {
    ref.point = apply(design[, control$y.name], 2, max)
    ref.point = ref.point + control$multicrit.ref.point.offset
  }
  
  if (control$multicrit.ref.point.method == "front") {
    front = nondominated_points(t(design[, control$y.name]))
    # Stupid check because emoa can drop to a vector
    if (is.vector(front))
      front = matrix(front, ncol = 1)
    ref.point = apply(front, 1, max)
    ref.point = ref.point + control$multicrit.ref.point.offset
  }
  
  return(ref.point)
}
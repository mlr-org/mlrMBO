# do global stuff that an algorithm needs for its initialization

mboAlgoInit = function(par.set, opt.path, control) {
  init = list()
  if (control$number.of.targets > 1L && control$multicrit.method == "parego") {
    # calculate all possible weight vectors and save them
    all.possible.weights = combWithSum(control$multicrit.parego.s, control$number.of.targets) /
      control$multicrit.parego.s
    # rearrange them a bit - we want to have the margin weights on top of the matrix
    # tricky: all margin weights have maximal variance
    vars = apply(all.possible.weights, 1, var)
    init$all.possible.weights =
      rbind(diag(control$number.of.targets), all.possible.weights[!vars == max(vars),])
  }
  return(init)
}

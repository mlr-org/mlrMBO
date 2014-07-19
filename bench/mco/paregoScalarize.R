source("bench/mco/WFG.R")

makePlot = function (rho, lambda, scale = 5.2) {
  sequence = seq(0, scale, length.out = 100)
  y = expand.grid(sequence, sequence)
  weighted.y = t(t(y) * lambda)
  
  y.star = apply(weighted.y, 1, max) + rho * rowSums(weighted.y)
  y.star = matrix(y.star, nrow = 100)
  contour(sequence, sequence, y.star)
  apply(y.star, 1, which.min)
  
  
  lines(lambda[2] * seq(0, scale, 0.01), lambda[1] * seq(0, scale, 0.01), col = "red", lwd = 2)
  f = function(x, lambda, rho, const) {
    y1 = (const - lambda[1] * x * (1 + rho)) / (rho * lambda[2])
    y2 = (const - lambda[1] * x * rho) / ((1 + rho) * lambda[2])
    pmin(y1, y2)
  }
  x = scale * 0.85 * lambda[2:1]
  const = max(lambda * x) + rho * lambda * x
  lines(sequence, f(sequence, lambda, rho, const), col = "blue", lwd = 2)
  
}


makePlot(0.05, c(0.9, 0.1))
points(t(wfg4pf(2, 100)), type = "l", lwd = 2)

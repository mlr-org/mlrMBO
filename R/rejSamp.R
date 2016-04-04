# #' Generate random param settings by rejection sampling
# #'
# #' @param f [\code{function}]\cr
# #'  Function to be interpreted as probability density function.
# #' @param n  [\code{integer(1)}]\cr
# #'  Amount of random numbers to be generated. Default is 1.
# #' @param par.set [\code{ParamSet}]\cr
# #'  ParamSet the generated random numbers are from.
# #' @param ...
# #'  Arguments passed to \code[sampleValue].
# #' @return
# #'  \code{n} Random numbers from the function \code{f} interpreted as probality density function.

rejSamp = function(f, n = 1, par.set, f.max, f.min, max.iter = 50000, ...) {
  assertFunction(f)
  assertCount(n, positive = TRUE)
  assertClass(par.set, "ParamSet")

  rej.helper = function(max.iter) {
    j = TRUE
    i = 1
    while(j && i <= max.iter) {
      x = generateRandomDesign(n = 1, par.set = par.set, ...)
      z = runif(1, f.min, f.max)
      if (f(x) >  z) {
        return(x)
      }
      i = i + 1
    }
    return(x)
  }
  
  return(replicate(n, rej.helper(max.iter), simplify = FALSE))
}

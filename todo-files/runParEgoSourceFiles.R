## The fabulous lz1-function to test the optimization
lz1 = function(x){
  f = numeric(2)
  n = length(x)
  J1 = seq(3, n, by = 2)
  J2 = seq(2, n, by = 2)
  f[1] = x[1] + 2 / length(J1) * 
    sum((x[J1] - x[1]^(0.5 * (1 + 3 * (J1 - 2) / (n - 2) )))^2)
  f[2] = 1 - sqrt(x[1]) + 2 / length(J2) *
    sum((x[J2] - x[1]^(0.5 * (1 + 3 * (J2 - 2) / (n - 2) )))^2)
  return(f)
}
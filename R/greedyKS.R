# Knapsack value
minQKSValue = function(p, P, sel){
  sum(p[sel]) + sum(sapply(sel, function(i){
    min(P[i,sel])
  }))
}
# KSValue
KSValue = function(p,P,sel){
  sum(p[sel])
}

# upper half of the negative Profit Matrix
createMinQMatrix = function(distance, p){
  res = -(p * (1 - distance))
  res = pmax(res, t(res))
  res[lower.tri(res,diag = TRUE)] = 0
  return(res)
}

createProfitMatrix = function(p, xs, type = "std"){
  dist.xs = dist(xs)
  distance = as.matrix(dist.xs)/max(dist.xs)
  
  res = switch (type,
                std = outer(X = p, Y = p, FUN = "+")/4 * distance,
                neg = - outer(X = p, Y = p, FUN = "+")/4 * (1 - distance),
                negU = createMinQMatrix(distance, p))
  diag(res) = 0
  return(res)
}

# ralative value
relMinKSvalues = function (P ,w , sel, p){
  sapply(seq_along(w), function (i){
    (p[i] + min(P[i, sel], P[sel, i]))/w[i]
  })
}

#relative KS value
relKSvalue = function( P, w, sel, p){
  p/w
}

greedyMinKS = function(p, w, P, wl){
  gKS(p,w, P,wl, relMinKSvalues, minQKSValue)
}

greedyKS = function(p,w,wl){
  gKS(p,w,NULL,wl, relKSvalue, KSValue)
}

gKS = function(p,w,P,wl,ks_sel, ks_val){
  
  Candidates = seq_along(p)
  
  cSets = lapply(Candidates, function(C){
    Cw = w[C]
    for (i in seq_along(p)){
      vals = ks_sel(P, w, C, p)
      vals[C] = 0
      vals[Cw + w > wl] = 0
      if (max(vals)>0){
        index = which.max(vals)
        C = c(C, index)
        Cw = Cw + w[index]
      } else {
        break()
      }
    }
    C
  })
  Cvalues = vapply(cSets, function (selected){
    ks_val(p, P, selected)
  }, FUN.VALUE = numeric(1))
  res = as.numeric(cSets[[which.max(Cvalues)]])
  attr(res, "p") = max(Cvalues)
  return(res)
}
# create Profit Matrix
createMinQMatrix = function(distance, priorities){
  res = -(priorities * (1 - distance))
  res[lower.tri(res,diag = TRUE)] = 0
  #diag(res) = 0
  return(res)
}
# ralative value
minQKPvalues = function (MinQMatrix ,times, selected, priorities){
  sapply(seq_along(times), function (i){
    (priorities[i] + min(MinQMatrix[i, selected]) + min(MinQMatrix[selected, i]))/times[i]
  })
}
# Knapsack value
minQKPValue = function(priorities, MinQMatrix, selected){
  sum(priorities[selected]) + sum(sapply(selected, function(i){
    min(MinQMatrix[i,selected])
  }))
}

greedyMinQKP = function(priorities, times, xs, limit, quadratic = TRUE){
  if (quadratic){
    dist.xs = dist(xs)
    distance = as.matrix(dist.xs)/max(dist.xs)
    MinQMatrix = createMinQMatrix(distance, priorities)
  }else{
    MinQMatrix = 0 * as.matrix(dist.xs)
  }
  Candidates = seq_along(priorities)
  
  cSets = lapply(Candidates, function(C){
    Ctime = times[C]
    for (i in seq_along(priorities)){
      vals = minQKPvalues(MinQMatrix, times, C, priorities)
      vals[C] = 0
      vals[Ctime + times > limit] = 0
      if (max(vals)>0){
        index = which.max(vals)
        C = c(C, index)
        Ctime = Ctime + times[index]
      } else {
        break()
      }
    }
    C
  })
  Cvalues = vapply(cSets, function (selected){
    minQKPValue(priorities, MinQMatrix, selected)
  }, FUN.VALUE = numeric(1))
  res = as.numeric(cSets[[which.max(Cvalues)]])
  attr(res, "p") = max(Cvalues)
  return(res)
}

expensiveMinQKP = function(priorities, times, xs, limit) {
  n = length(priorities)
  cs = lapply(seq_len(n), function(x) {
    res = combn(n, x)
    unlist(apply(res, 2, list), FALSE)
  })
  cs = unlist(cs, FALSE)
  
  TValues = vapply(cs, function(selected) {
    sum(times[selected])
  }, FUN.VALUE = numeric(1))
  
  cs = cs[TValues<=limit]
  
  dist.xs = dist(xs)
  distance = as.matrix(dist.xs)/max(dist.xs)
  MinQMatrix = createMinQMatrix(distance, priorities)
  
  CValues = vapply(cs, minQKPValue, priorities = priorities, MinQMatrix = MinQMatrix, FUN.VALUE = numeric(1))
  max.CValue = max(CValues)
  res = cs[[which.max(CValues)]]
  attr(res, "p") = max.CValue
  return(res)
}
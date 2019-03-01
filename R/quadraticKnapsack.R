# create Profit Matrix
createQMatrix = function(distance, priorities){
  res = - outer(X = priorities, Y = priorities, FUN = "+")/4 * (1 - distance)^4
  mat1 = res
  mat2 = res
  mat1[lower.tri(mat1, diag = TRUE)] = 0
  mat2[upper.tri(mat2, diag = TRUE)] = 0
  for (i in 1:nrow(res)){
    sel1 = mat1[i,] != min(mat1[i,])
    mat1[i,sel1] = 0
    sel2 = mat2[,i] != min(mat2[,i])
    mat2[sel2,i] = 0
  }
  res[upper.tri(res)] = mat1[upper.tri(res)]
  res[lower.tri(res, diag = TRUE)] = mat2[lower.tri(res, diag = TRUE)]
  return(res)
}
# ralative value
relQKPvalues = function (QMatrix ,times, selected, priorities){
  (priorities + colSums(QMatrix[selected, , drop = FALSE]) + rowSums(QMatrix[,selected , drop = FALSE])) / times
}
# Knapsack value
QKPValue = function(priorities, QMatrix, selected){
  sum(priorities[selected]) + sum(QMatrix[selected, selected])
}

greedyQKP = function(priorities, times, xs, limit, quadratic = TRUE){
  dist.xs = dist(xs)
  distance = as.matrix(dist.xs)/max(dist.xs)
  if (quadratic){
    QMatrix = createQMatrix(distance, priorities)
  }else{
    QMatrix = 0 * as.matrix(dist.xs)
  }
  Candidates = seq_along(priorities)
  
  cSets = lapply(Candidates, function(C){
    Ctime = times[C]
    for (i in seq_along(priorities)){
      vals = relQKPvalues(QMatrix, times, C, priorities)
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
    QKPValue(priorities, QMatrix, selected)
  }, FUN.VALUE = numeric(1))
  res = as.numeric(cSets[[which.max(Cvalues)]])
  attr(res, "p") = max(Cvalues)
  return(res)
}

expensiveQKP = function(priorities, times, xs, limit) {
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
  QMatrix = createQMatrix(distance, priorities)
  
  CValues = vapply(cs, QKPValue, priorities = priorities, QMatrix = QMatrix, FUN.VALUE = numeric(1))
  max.CValue = max(CValues)
  res = cs[[which.max(CValues)]]
  attr(res, "p") = max.CValue
  return(res)
}

QKPvalues = function (QMatrix,times, selected){
  sapply(seq_along(times), function (i){
    (QMatrix[i,i] + sum(QMatrix[i, selected]))/times[i]
  })
}

QKPresValue = function(QMatrix, selected){
  sum(sapply(selected, function(i){
    QMatrix[i, selected]
  }))
}

greedyQKP = function(priorities, times, xs, limit){
  distance = as.matrix(dist(xs))/max(dist(xs))
  QMatrix = distance * priorities
  for( i in seq_along(priorities)){
    QMatrix[i,i] = priorities[i]
  }
  
  Candidates = seq_along(priorities)
  Ctimes = times
  Cvalues = numeric(length(priorities))
  
  cSets = lapply(Candidates, function(C){
    for (i in seq_along(priorities)){
      vals = QKPvalues(QMatrix, times, C)
      vals[unlist(C)] = 0
      vals[Ctimes[C[1]] + times > limit] = 0
      if (max(vals)>0){
        index = which.max(vals)
        C = c(C, index )
        Ctimes[C[1]] = Ctimes[C[1]] + times [index]
      } else {
        break()
      }
    }
    C
  })
  Cvalues = lapply(cSets, function (selected){
    QKPresValue(QMatrix, selected)
  })
  return (cSets[[which.max(Cvalues)]])
}
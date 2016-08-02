# proposes 1 or multi-points with the EPIC-method:

# Predict probability of beeing non-dominated for each indiviual
# from the sampled parameter space and evaluate the one whose probability
# is closest to p_next

proposePointsEPIC = function(opt.state) {
  
  opt.problem = getOptStateOptProblem(opt.state)
  models = getOptStateModels(opt.state)$models[[1]]
  par.set = getOptProblemParSet(opt.problem)
  control = getOptProblemControl(opt.problem)
  opt.path = getOptStateOptPath(opt.state)
  iter = getOptStateLoop(opt.state)
  S = opt.problem$sampled.decision.space
  
  st = system.time({
    # Predict with all points in S
    p = getPredictionProbabilities(predict(models, newdata = S))
    
    # All valid points whose non-dom probability is high enough
    p = ifelse(p < control$multicrit.epic.p.nond, -1, p)
    
    if (control$propose.points == 1L) {
      # Single Point is easy
      candidates = which.min(abs(p - control$multicrit.epic.p.next))
    } else {
      # Multi Point is not. Here we want the set of points that has the smallest
      # distance to the p.next vector. calculate the pairwise dists between p and
      # control$multicrit.epic.p.next and their order
      dists = sapply(p, function(x) abs(x - control$multicrit.epic.p.next))
      dists.order = apply(dists, 1, order)

      # initialize each candidate as the nearest point to a specific p.nest
      rows = rep(1, length(control$multicrit.epic.p.next))
      candidates = dists.order[1, ] # the smallest distances between the vectors
      
      # ideally, we are finished now. unfortunately, there could be duplicates
      # in index and we do not want to evaluate the same point twice
      # if there are duplicated indices in index, then we have to find some other
      # indices for the duplicated ones.
      while(any(duplicated(candidates))){
        conflicts = duplicated(candidates) | duplicated(candidates, fromLast = TRUE)
        
        # take one conflict in each iteration
        conflict.candidates = unique(candidates[conflicts])[1]
        
        current = which(candidates == conflict.candidates)
        
        # for the conflicted indices determine the next best index
        candidates.new = sapply(current, function(x) dists.order[rows[x] + 1, x])
        
        # keep the one which would have most deterioration, the others are set to
        # the indices which have the next smallest distances
        keep = which.max(abs(dists[current, conflict.candidates] - diag(dists[current, candidates.new])))
        candidates[current[-keep]] = candidates.new[-keep]
        rows[current[-keep]] = rows[current[-keep]] + 1
      }
    }
    
    crit.vals = p[candidates]
    prop.points = S[candidates, ]
    # remove candidate from S
    opt.problem$sampled.decision.space = S[-candidates, ]
  })
  
  return(list(prop.points = prop.points, propose.time = st[3L],
    prop.type = rep("epic", control$propose.points), crit.vals = crit.vals,
    errors.model = NA_character_))
}

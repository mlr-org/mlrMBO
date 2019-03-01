proposePointsGroupInfill = function(opt.state) {
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  m = control$propose.points
  n = control$multipoint.groupinfill.sample.n
  xs = generateRandomDesign(par.set = getOptProblemParSet(opt.problem), n*m)
  xs = convertDataFrameCols(xs, ints.as.num = TRUE, logicals.as.factor = TRUE)
  xs = split(xs, rep(seq_len(n), each = m))
  group.infill.crit = simpleGroupInfillCrit
  st = system.time({
    infill.results = parallelMap(group.infill.crit, points = xs, level = "mlrMBO.propose.points", more.args = list(opt.state = opt.state))
  })
  infill.crits = extractSubList(infill.results, "infill.crit", simplify = TRUE)
  best.index = which.min(infill.crits)
  prop.points = xs[[best.index]]
  list(
    prop.points = prop.points,
    propose.time = c(st[3L], rep(0, times = m-1L)),
    prop.type = rep(paste0("grouped.infill_", control$infill.crit), times = m),
    crit.vals = infill.results[[best.index]]$single.crit.vals,
    crit.components = infill.results[[best.index]]$crit.components,
    errors.model = NA_character_
  )
}


simpleGroupInfillCrit = function(points, opt.state, ...) {
  models = getOptStateModels(opt.state)$models
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  infill.crit.fun = getInfillCritFunction(control$infill.crit)
  par.set = getOptProblemParSet(opt.problem)
  models = if (inherits(models, "WrappedModel")) list(models) else models
  opt.path = getOptStateOptPath(opt.state)
  design = convertOptPathToDf(opt.path, control)
  iter = iter = getOptStateLoop(opt.state)
  crit.vals = infill.crit.fun(points, models, control, par.set, design, iter, attributes = TRUE, ...)
  infill.sum = sum(crit.vals)
  dist.penalty = mean(cluster::daisy(points, metric = "gower"))
  list(
    infill.crit = infill.sum * control$multipoint.groupinfill.dist.penalty * dist.penalty,
    single.crit.vals = crit.vals,
    crit.components = attr(crit.vals, "crit.components")
  )

}

# proposePointsQuadraticKnapsack = function(opt.state) {
#   opt.problem = getOptStateOptProblem(opt.state)
#   control = getOptProblemControl(opt.problem)
#   infill.crit.fun = getInfillCritFunction(control$infill.crit)
#   models = getOptStateModels(opt.state)$models
#   models = if (inherits(models, "WrappedModel")) list(models) else models
#   par.set = getOptProblemParSet(opt.problem)
#   design = convertOptPathToDf(opt.path, control)
#   iter = iter = getOptStateLoop(opt.state)

#   m = control$propose.points
#   n = control$multipoint.groupinfill.sample.n

#   xs = generateRandomDesign(par.set = getOptProblemParSet(opt.problem), n)

#   profits = infill.crit.fun(xs, models, control, par.set, design, iter, attributes = TRUE, ...)

#   distances = as.matrix(cluster::daisy(xs, metric = "gower"))

#   cancelation = outer(profits, profits, FUN = function(X,Y) X + Y) * exp(-distances)

#   callQKFunction(profits, cancelation, TIME???)
#   # get in return
#   # list with jobs for each CPU
#   # order in how they should be executed
#   # cpu1 = c(590, 4, 99, 188)
#   # cpu2 = c(110, 1, 160, 55)
#   # ...
# }

proposePointsQKPCB = function(opt.state){
  opt.problem = getOptStateOptProblem(opt.state)
  control = getOptProblemControl(opt.problem)
  
  # returns as many points as specified
  raw.points = proposePointsParallelCB(opt.state)
  
  propose.time = raw.points$propose.time[1]
  raw.points$propose.time[1] = NA
  
  time.model = getOptStateTimeModel(opt.state)
  time.prediction = predict(time.model, newdata = raw.points$prop.points)
  predicted.time = getPredictionResponse(time.prediction)
  predicted.time.se = getPredictionSE(time.prediction)
  
  # repair negative times
  # replace negative times with smallest observed time or smallest postive predicted time
  min.time = min(predicted.time[predicted.time > 0], getOptPathExecTimes(getOptStateOptPath(opt.state)), na.rm = TRUE)
  predicted.time[predicted.time <= 0] = min.time

  if(is.null(predicted.time.se))
    predicted.time.se = rep(NA_real_, length(predicted.time))
  
  if (control$multipoint.method == "cb" && control$schedule.priority == "exploit") {
    priorities = -raw.points$multipoint.cb.lambdas - min(-raw.points$multipoint.cb.lambdas) + 0.1
  }else if(control$multipoint.method == "cb" && control$schedule.priority == "explore"){
    priorities = raw.points$multipoint.cb.lambdas - min(raw.points$multipoint.cb.lambdas) + 0.1
  }else if(control$multipoint.method == "cb" && control$schedule.priority == "balanced"){
    priorities = -abs(log(prop$multipoint.cb.lambdas) - log(control$infill.crit.cb.lambda)) - min(-abs(log(prop$multipoint.cb.lambdas) - log(control$infill.crit.cb.lambda))) + 0.1
  }else if(control$schedule.priority == "infill"){
    priorities = -raw.points$crit.vals - min(-raw.points$crit.vals) + 0.1
    priorities = priorities[,1]
  }else if (control$schedule.priority == "raw"){
    priorities = -raw.points$crit.components$mean - min(-raw.points$crit.components$mean) + 0.1
  }else {
    stopf("Schedule Priority mehtod %s was not appliable!", control$schedule.priority)
  } 
    
    
  t.max = predicted.time[which.max(priorities)] + predicted.time.se[which.max(priorities)]
  
  if (control$schedule.ks == "cluster"){
    predicted.time[predicted.time > t.max] = t.max * control$schedule.nodes + 1 # TODO more efficient solution
    priorities = distanceCluster(priorities = priorities, raw.points, opt.state = opt.state)
    sel.points = greedyKS(priorities, predicted.time,t.max * control$schedule.nodes)
  } else if (control$schedule.ks ==  "clusterFF"){
    priorities = distanceCluster(priorities = priorities, raw.points, opt.state = opt.state)
    p.order = order(priorities, decreasing = TRUE)
    occupied.time = 0
    sel.points = rep(FALSE, length(priorities))
    for (i in p.order){
      if((predicted.time[i] + occupied.time) <= (t.max * control$schedule.nodes) && predicted.time[i] <= t.max){
        sel.points[i] = TRUE
        occupied.time = occupied.time + predicted.time[i]
      }
    }
  } else if(control$schedule.ks ==  "cancel"){
    predicted.time[predicted.time > t.max] = t.max * control$schedule.nodes + 1 # TODO more efficient solution
    P = createProfitMatrix(priorities, raw.points$prop.points, "negU")
    sel.points = greedyMinKS(priorities, predicted.time,  P,t.max * control$schedule.nodes)
  } else if(control$schedule.ks == "fixCancel"){
    predicted.time[predicted.time > t.max] = t.max * control$schedule.nodes + 1 # TODO more efficient solution
    best = which.max(priorities)
    pt = predicted.time
    pt[best] = t.max * control$schedule.nodes + 1
    sel.points = greedyMinKS(priorities, pt, createProfitMatrix(priorities, raw.points$prop.points, "negU") ,(t.max * (control$schedule.nodes-1)))
    sel.points = c(sel.points, best)
  } else if(control$schedule.ks == "fixCluster"){
    predicted.time[predicted.time > t.max] = t.max * control$schedule.nodes + 1 # TODO more efficient solution
    priorities = distanceCluster(priorities = priorities, raw.points, opt.state = opt.state)
    best = which.max(priorities)
    pt = predicted.time
    pt[best] = t.max * control$schedule.nodes + 1
    sel.points = greedyKS(priorities, pt, (t.max * (control$schedule.nodes-1)))
    sel.points = c(sel.points, best)
  } else {
    stop("no valid group Infill")
  }
  
  
  res = list()
  res$prop.points = raw.points$prop.points[sel.points,]
  res$propose.time = raw.points$propose.time[sel.points]
  res$train.time = raw.points$train.time[sel.points]
  res$predicted.time = predicted.time[sel.points]
  res$predicted.time.se = predicted.time.se[sel.points]
  res$multipoint.cb.lambdas = raw.points$multipoint.cb.lambdas[sel.points]
  res$crit.vals = raw.points$crit.vals[sel.points]
  res$prop.type = raw.points$prop.type[sel.points]
  res$errors.model = raw.points$errors.model[sel.points]
  res$crit.components = raw.points$crit.components[sel.points,]
  res$filter.replace = rep(FALSE,length(sel.points))
  res$t.max = t.max
  
  res$propose.time[1] = propose.time
  return(res)
}

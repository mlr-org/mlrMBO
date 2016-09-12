# Evaluates the target function using parallelMap
# clusters the points for better distributed point evaluation 
# @param wrapFun [\code{function}] \cr
#   A function which evaluates one item of xs and returns a list with the \code{y}, \code{time} and \code{user.extras}
# @param xs [\code{list}] \cr
#   A list of the x values to evaluate by \code{wrapFun}
# @param xs.schedule.info [\code{list}] \cr
#   A list containing vectors of the same length as \code{xs} giving the estimated times, priorities and times.se for each evaluation of \code{x}.
# @param opt.state [\code{OptState}]\cr
# @return [\code{list}] \cr
#   List containing the results of \code{wrapFun} for each item in xs


evalScheduleCluster = function(wrapFun, xs, xs.trafo, xs.schedule.info = NULL, extras = NULL, opt.state) {
  if (!is.null(xs.schedule.info$times)) {
    #copy stuff
    control = getOptProblemControl(getOptStateOptProblem(opt.state))
    schedule.nodes = control$schedule.nodes
    xs = xs
    xs.trafo = xs.trafo
    xs.schedule.info = xs.schedule.info
    extras = extras
    #options
    des.jobs = min(schedule.nodes * 3,length(xs.schedule.info$priorities))
    min.prio = min(xs.schedule.info$priorities)
    if (min.prio < 0){
      x.prios = xs.schedule.info$priorities - min.prio * 1.01  #FIXME: nicer?  
    }else{
      x.prios =  xs.schedule.info$priorities 
    }	
    # format proposed points
    prop.points = data.frame()
    for(i in seq_along(xs)){
      prop.points = rbind (prop.points, as.data.frame(xs[i]))
    }

    
    #cluster the jobs
    clu = hclust(dist(prop.points))
    gtree = cutree(clu, k = des.jobs)
    
    # initialize new Variables
    new.sched = data.frame(
      times = numeric(),
      priorities = numeric(),
      times.se = numeric()
    )
    new.extras = list()
    new.xs = list()
    new.xs.trafo = list()
    
    #copy the best job's per partition for scheduling
    for(k in 1:des.jobs){
      cluster.mask = x.prios * (gtree == k)
      pos.sel = which.max(cluster.mask)
      new.sched = rbind(new.sched, xs.schedule.info[pos.sel,])
      new.extras[length(new.extras) + 1] = extras[pos.sel]
      new.xs[length(new.xs) + 1] = xs[pos.sel]
      new.xs.trafo[length(new.xs.trafo) + 1] = xs.trafo[pos.sel] 
    }

    evalScheduleKnapsack(wrapFun = wrapFun, xs = new.xs, xs.trafo = new.xs.trafo, xs.schedule.info = new.sched, extras = new.extras, opt.state = opt.state)
  }else{
    evalScheduleParallelMap(wrapFun = wrapFun, xs = xs, xs.trafo = xs.trafo, xs.schedule.info = xs.schedule.info, extras = extras, opt.state = opt.state)
  }
}
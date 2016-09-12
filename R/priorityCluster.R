# Determines priorities by clustering the proposed points by their distances in the X space for better distributed priorities.
#
# @param priorities[\code{numeric} \cr
#   A Vector containing the priorities for the points in prop
# @param prop [\code{list}] \cr
#   A list containing the proposed points
# @param opt.state [\code{OptState}]\cr
#@return [\code{numeric} Numeric vector of priorities
#   Vector containing the priorities determined by clustering


priorityCluster = function(priorities, prop, opt.state) {
  prop = prop
  control = getOptProblemControl(getOptStateOptProblem(opt.state))
  schedule.nodes = control$schedule.nodes
    
  # use infil crit for priorities
  priorities = priorities
  prop.points = prop$prop.points
    
  # order everything according to priorities 
  order.idx = order(priorities, decreasing = TRUE)
  priorities = priorities[order.idx]
  prop.points = prop.points[order.idx,]

  #options
  no.jobs = length(priorities)
  # an average of 3 Jobs per cluster
  des.clusters = max(round(no.jobs / 3),1)
  

  #cluster the jobs
  clu = hclust(dist(prop.points))
  gtree = cutree(clu, k = des.clusters)
   
  cluster.mask = logical(no.jobs)
  av.jobs = logical(no.jobs)
  job.mask = logical(no.jobs)
  job.order = numeric()
  av.jobs[] = TRUE
  
  # take the first available job per cluster (priority)
  # add them to the job order and remove them from available Jobs
  
  while (length(job.order) < no.jobs){
    job.mask[] = FALSE
    for (i in 1:des.clusters){
      cluster.mask = (gtree == i) & av.jobs
      pos.sel = which.max(cluster.mask)
      if (cluster.mask[pos.sel]){
        job.mask[pos.sel] = TRUE
        av.jobs[pos.sel] = FALSE
      }
    }
    job.order = c(job.order, which(job.mask))
  }
  
  # Order Jobs according to Job order and assign priorities
  new.prio = no.jobs:1
  priorities = new.prio[order(job.order)]

  priorities[order(order.idx)]
}
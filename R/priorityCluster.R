# Refinement of job priorities to optimize job selection for
# resource-aware parallel execution in an MBO-iteration.
# Proposed points (jobs) are clustered by their distances in the domain space.
# The priorities of candidates that have similar parameter settings are
# decreased while priorities of candidates with different parameter 
# configurations are increased to avoid unnecessary evaluations of very 
# similar configurations.
# Number of clusters < number of jobs
# The new priorities serve as input for scheduling.
#
# @param priorities[\code{numeric} \cr
#   A Vector containing the priorities for the points in prop
# @param prop [\code{list}] \cr
#   A list containing the proposed points
# @param opt.state [\code{OptState}]\cr
# @return [\code{numeric} Numeric vector of priorities
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

  # options
  no.jobs = length(priorities)
  # an average of 3 Jobs per cluster
  des.clusters = max(round(no.jobs / 3),1)
  

  # cluster jobs by their euklidean distances in the domain space.
  # number of clusters < proposed points
  clu = hclust(dist(prop.points))
  gtree = cutree(clu, k = des.clusters)
   
  cluster.mask = logical(no.jobs)
  av.jobs = logical(no.jobs)
  job.mask = logical(no.jobs)
  job.order = numeric()
  av.jobs[] = TRUE
  
  # select jobs with the highest priority from each cluster
  # sort jobs according to their priority and add them to 
  # the new list of selected jobs
  # added jobs are removed from the clusters and empty clusters are eliminated
  # repeat until all jobs are in the new list of selected jobs
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
  
  # assign new priorities based on the order of new list (job.order)
  # first job gets the highest priority, last job gets lowest
  new.prio = no.jobs:1
  priorities = new.prio[order(job.order)]

  priorities[order(order.idx)]
}
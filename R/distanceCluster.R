# Refinement of job priorities to optimize job selection for
# resource-aware parallel execution in an MBO-iteration.
# Proposed points (jobs) are clustered by their distances in the domain space.
# The priorities of candidates that have similar parameter settings are
# decreased while priorities of candidates with different parameter 
# configurations are increased to avoid unnecessary evaluations of very 
# similar configurations.
# Number of clusters = number of jobs
# The new priorities serve as input for scheduling.
#
# @param priorities[\code{numeric} \cr
#   A Vector containing the priorities for the points in prop
# @param prop [\code{list}] \cr
#   A list containing the proposed points
# @param opt.state [\code{OptState}]\cr
# @return [\code{numeric} Numeric vector of priorities
#   Vector containing the priorities determined by clustering

distanceCluster = function(priorities, prop, opt.state) {
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
  
  if (nrow(prop.points) != no.jobs){
    messagef("Error with prios")
    messagef(priorities)
    messagef(prop.points)
    stop("prios != points")
  }
  # hierarchical clustering
  # number of clusters = number of proposed points
  clu = hclust(dist(prop.points))
  used.jobs = numeric()
  cluster.mask = logical(no.jobs)
  job.order = 1:no.jobs
  used.jobs = 1

  # the job with highest priority gets first position in the new 
  # priority list (used.jobs), all jobs are split into i clusters 
  # generats an ordering where each job has the highest priority of the
  # most distant cluster to its predecessor following the hierarchical structure
  for (i in 2:no.jobs){                               
    gtree = cutree(clu, k = i)
    cluster.mask[]  = TRUE
    # masks clusters already containing a job with a new positon
    for (j in 1:length(used.jobs)){                   
      for (k in 1:i){                                 
        if(used.jobs[j] %in% job.order[gtree == k]){
          cluster.mask[gtree == k] = FALSE   
        }
      }
    }
    used.jobs = c(used.jobs, which.max(cluster.mask))
  }
  
  # assign new priorities based on the new order of the list (used.jobs)
  # first job gets the highest priority, last job gets lowest
  new.prio = no.jobs:1
  priorities = new.prio[order(used.jobs)]
  
  priorities[order(order.idx)]
}

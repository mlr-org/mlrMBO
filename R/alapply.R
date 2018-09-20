library(parallel)

startJob = function(coreid, sched.info, X, FUN){
  if(any(sched.info$ava)){
    if(any(sched.info$ava[,coreid])){
      # choose task that is ready for execution
      jobid <- which(sched.info$ava[,coreid])[1]
    } else {
      # steal job from other core
      ava = sapply(X = seq_along(sched.info$ava[,1]), FUN = function (i) any(sched.info$ava[i,]))
      jobid = which.max(ava)
    }
    sched.info$ava[jobid,] = FALSE
    sched.info$job.id[coreid] = jobid
    waiting = which(sched.info$waiting == jobid)
    if (length(waiting) == 1){
      # paused task
      sched.info$jobs[coreid] = sched.info$waiting.jobs[waiting]
      sched.info$ctime[waiting] = proc.time()[3]
      sched.info$job.pid[coreid] = sched.info$jobs[[coreid]]$pid
      system(paste0("kill -CONT ", sched.info$job.pid[coreid]), intern = TRUE)
    }else{
      # new task
      sched.info$jobs[coreid] = list(mcparallel(FUN(X[[jobid]]),
                                                mc.set.seed = TRUE,
                                                silent = FALSE))
      sched.info$job.pid[coreid] = sched.info$jobs[[coreid]]$pid
    }
  }
  return(sched.info)
}


alapply = function (X, scheduled.on, wait.at, FUN){
  messagef("start job Execution")
  mccollect(wait = FALSE, timeout = 1)
  sched.info = list()
  sx <- seq_along(X)
  res <- vector("list", length(sx))
  
  hcpu = max(unlist(x = scheduled.on,recursive = TRUE))
  cpu.map= lapply(sx, function (i){
    data = vector(mode= "logical",length = hcpu)
    data[as.vector(scheduled.on[[i]])] = TRUE
    data
  })
  sched.info$ava = do.call(rbind,cpu.map)
  fin <- rep(FALSE, length(X))
  sched.info$job.id = numeric(hcpu)
  # choose first item for each core to start
  # delete unused cores off the matrix
  for (i in 1:hcpu){
    sched.info$job.id[i] = which(sched.info$ava[,i])[1]
    sched.info$ava[sched.info$job.id[i],] = FALSE
  }
  unusedCores = which(is.na(sched.info$job.id))
  sched.info$job.id = na.omit(sched.info$job.id)
  if (length(unusedCores) > 0) sched.info$ava = sched.info$ava[,-unusedCores, drop = FALSE]
  sched.info$jobs <- lapply(sched.info$job.id,
                            function(i) mcparallel(FUN(X[[i]]),
                                                   mc.set.seed = TRUE,
                                                   silent = FALSE))
  sched.info$job.pid <- sapply(X = sched.info$jobs,FUN = function(j) j$pid)
  
  start.time = proc.time()[3]
  next.change = start.time + min(wait.at)
  sched.info$waiting = numeric(length(sched.info$job.id))
  sched.info$stime = numeric(length(sched.info$job.id))
  sched.info$ctime = numeric(length(sched.info$job.id))
  sched.info$waiting.jobs = vector("list", length(sched.info$job.id))
  while (!all(fin)) {
    # collect finished jobs
    # tmp = mccollect(sched.info$jobs[!is.na(sched.info$jobs)], wait = FALSE, timeout = 1)
    tmp = mccollect(wait = FALSE, timeout = 1)
    while (!is.null(tmp) && length(tmp) > 0){
      if(is.null(tmp[[1]])){
        tmp = tmp[-1]
        next()
      }
      coreid = which(sched.info$job.pid == names(tmp)[[1]])
      if (length(coreid) == 1){
        jobid = sched.info$job.id[coreid]
        res[[jobid]] = tmp[[1]]
        fin[jobid] = TRUE
        sched.info$job.pid[coreid] = NA
        sched.info$jobs[coreid] = NA
        sched.info$job.id[coreid] = NA
        tmp = tmp[-1]
        sched.info = startJob(coreid, sched.info, X, FUN)
      }else{
        wid = which(sapply(X = sched.info$waiting.jobs, FUN = function (j){ 
          if (!is.null(j)){
            return(j$pid == names(tmp)[[1]])
          } else {
            return(FALSE)
          }
        }))
        sched.info$ava[sched.info$waiting[wid],] = FALSE
        fin[sched.info$waiting[wid]] = TRUE
        sched.info$stime[wid] = 0
        res[[sched.info$waiting[wid]]] = tmp[[1]]
        tmp = tmp[-1]
        system(paste0("kill -KILL ", sched.info$waiting.jobs[[wid]]$pid))#, intern = TRUE) #any other way?
      }
    }
    # pause Jobs
    while(proc.time()[3] >= next.change){
      jobid = which.min(wait.at)
      wait.at[jobid] = Inf
      next.change = start.time + min(wait.at)
      coreid = which(sched.info$job.id == jobid)
      if(length(coreid) == 1){
        jobpid = sched.info$job.pid[coreid]
        if(is.na(jobpid)){
          stop("job pid not found, This should not happen", call. = FALSE)
        }
        system(paste0("kill -STOP ", jobpid), intern = TRUE)
        sched.info$waiting[coreid] = jobid
        sched.info$waiting.jobs[[coreid]] = sched.info$jobs[[coreid]]
        sched.info$stime[[coreid]] = proc.time()[3]
        sched.info$ava[jobid, scheduled.on[[jobid]][2]] = TRUE
        sched.info$job.pid[coreid] = NA
        sched.info$jobs[coreid] = NA
        sched.info$job.id[coreid] = NA
        sched.info = startJob(coreid, sched.info, X, FUN)
      }
    }
  }
  # end finished childprocesses
  mccollect()
  for (i in seq_along(sched.info$waiting)){
    job.id = sched.info$waiting[[i]]
    if (job.id > 0){
      wtime = sched.info$ctime[i] - sched.info$stime[i]
      res[[job.id]]$time = res[[i]]$time - wtime
      res[[job.id]]$user.extras$exec.time.real = res[[i]]$user.extras$exec.time.real - wtime
      attr(res[[job.id]]$y,"exec.time") = attr(res[[i]]$y,"exec.time") - wtime
      res[[job.id]]$stime = sched.info$stime[i] - start.time
      res[[job.id]]$ctime = sched.info$ctime[i] - start.time
    }
  }
  return(res)
}
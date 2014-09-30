setwd("/home/bischl/cos/mlrMBO/")

library(BatchExperiments)
library(checkmate)
load_all("/home/bischl/cos/ParamHelpers/todo-files/eaf/")

source("/home/bischl/cos/ParamHelpers/todo-files/plotEAF.R")

# reg = loadRegistry("/home/bischl/cos/mco_bench-files/", work.dir = "~/cos/mlrMBO/")

job.info = getJobInfo(reg, pars = TRUE)
job.info$algo2 = paste(job.info$algo, job.info$budget, job.info$prop.points,
  job.info$indicator, job.info$crit, sep = "-")
job.info$algo2 = str_replace_all(job.info$algo2, "-NA", "")

pids = c("GOMOP3_3D2M", "GOMOP_2D2M", "GOMOP_5D2M", "dtlz2_5D2M", "zdt1_5D2M", "zdt2_5D2M", "zdt3_5D2M")
aids = c("nsga2-normal", "dib-1-eps", "dib-1-sms", "parego-1-ei", "parego-4-lcb", "mspot-1-mean", "mspot-4-lcb")

getOptPaths = function(job.info, pids, aids) {
  ops = list()
  for (pid in pids) {
    for (aid in aids) {
      jids = job.info[job.info$prob == pid & job.info$algo2 == aid, "id"]
      assertInteger(jids, len = 20L, any.missing = FALSE)
      ops[[pid]][[aid]] = extractSubList(loadResults(reg, jids), "opt.path", simplify = FALSE)
    }
    names(ops[[pid]]) = paste(seq_along(aids), aids)
  }
  return(ops)
}


plotMMBOEAF = function(prob.id, opt.paths, file = NULL, logscale = FALSE, title = prob.id, xlim = NULL, ylim = NULL, ...) {
  # col = rainbow(length(aids))
  if (!logscale) {
    log = ""; legend.pos = "topright"
  } else {
    log = "xy"; legend.pos = "bottomleft"
  }
  if (!is.null(file)) pdf(file)
  d = plotEAF(opt.paths[[prob.id]], legend.pos = "topright", log = log, main = title,
    xlab = "y1", ylab = "y2", ...)
  if (!is.null(file)) dev.off()
  return(d)
}

# opt.paths = getOptPaths(job.info, pids, aids)

lty  = c("dashed", "solid", "solid", "solid",   "dashed", "solid", "dashed", "solid")
col  = c("red",   "pink",  "green",   "yellow",   "grey",   "blue",  "black", "orange")

for (pid in pids) {
  plotMMBOEAF(pid, opt.paths, file = sprintf("eaf_%s.pdf", pid))
}

load("results_trends.RData")
library(plyr)
library(stringr)
library(ggplot2)

makeTrendPlot = function(res, res.nsga2, res.rs, pid, indic, prop) {
  res = res[[indic]]
  # mean indicator contribution for each algo
  res = subset(res, res$prob == pid)
  res$algo2 = paste(res$algo, res$prop.points, res$indicator, res$crit, sep = "-")
  res$algo2 = str_replace_all(res$algo2, "-NA", "")
  res = ddply(res, "algo2", 
    function(x) colMeans(x[, 7:(ncol(res)-2)]))
  # reshape format of dataset
  reshaped = data.frame (
    algo2 = rep(res$algo2, each = ncol(res) - 1),
    dob = rep(0:(ncol(res) - 2), nrow(res)),
    indic = as.vector(t(as.matrix(res[, -1])))
  )
  
  # baseline
  nsga2 = subset(res.nsga2[[indic]], res.nsga2[[indic]]$prob == pid)
  nsga2$algo2 = "nsga2"
  nsga2 = ddply(nsga2, "algo2", 
    function(x) colMeans(x[, 6:(ncol(nsga2) - 1)]))
  baseline.nsga2 = data.frame(
    algo2 = rep("nsga2", ncol(nsga2) - 1),
    dob = seq(1, max(reshaped$dob), length.out = ncol(nsga2) - 1),
    indic = as.vector(as.matrix(nsga2[, 2:ncol(nsga2)]))
  )
  
  rs = subset(res.rs[[indic]], res.rs[[indic]]$prob == pid)
  rs$algo2 = "randomSearch"
  rs = ddply(rs, "algo2", 
    function(x) colMeans(x[, 6:(ncol(rs)-1)]))
  rs = rs[c(FALSE, !is.na(rs[-1]))]
  baseline.rs = data.frame(
    algo2 = rep("rs", ncol(rs) - 1),
    dob = seq(0, max(reshaped$dob), length.out = ncol(rs) - 1),
    indic = as.vector(as.matrix(rs[, 2:ncol(rs)]))
  )
  
  reshaped = rbind(reshaped, baseline.nsga2, baseline.rs)
  
  p =  ggplot(reshaped, aes(x = dob, y = indic, col = algo2))
  p = p + geom_line(lwd = 1.25) + xlab("Iteration") + ylab(paste("mean", indic, "in iteration"))
  p = p + ggtitle(paste("Trend for ", indic, " indicator of ", prop, "-point algorithms on ", pid, sep = ""))
  p
}

pdf("indicator_trends.pdf")
for (pid in c("GOMOP2_2D3M", "GOMOP_2D2M", "GOMOP_2D5M")) {
  print(pid)
  print(makeTrendPlot(res.2d.single, res.nsga2, res.rs, pid, "hv", prop = 1L))
  print(makeTrendPlot(res.2d.quad, res.nsga2, res.rs,   pid, "hv", prop = 4L))
  print(makeTrendPlot(res.2d.single, res.nsga2, res.rs, pid, "r2", prop = 1L))
  print(makeTrendPlot(res.2d.quad, res.nsga2, res.rs,   pid, "r2", prop = 4L))
  print(makeTrendPlot(res.2d.single, res.nsga2, res.rs, pid, "eps", prop = 1L))
  print(makeTrendPlot(res.2d.quad, res.nsga2, res.rs,   pid, "eps", prop = 4L))
}
for (pid in c("GOMOP_5D2M", "GOMOP_5D5M", "dtlz2_5D2M", "dtlz2_5D5M",
  "zdt1_5D2M", "zdt2_5D2M", "zdt3_5D2M")) {
  print(pid)
  print(makeTrendPlot(res.5d.single, res.nsga2, res.rs, pid, "hv", prop = 1L))
  print(makeTrendPlot(res.5d.quad, res.nsga2, res.rs,   pid, "hv", prop = 4L))
  print(makeTrendPlot(res.5d.single, res.nsga2, res.rs, pid, "r2", prop = 1L))
  print(makeTrendPlot(res.5d.quad, res.nsga2, res.rs,   pid, "r2", prop = 4L))
  print(makeTrendPlot(res.5d.single, res.nsga2, res.rs, pid, "eps", prop = 1L))
  print(makeTrendPlot(res.5d.quad, res.nsga2, res.rs,   pid, "eps", prop = 4L))
}
dev.off()

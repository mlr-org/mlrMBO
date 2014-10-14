load("results_trends.RData")
library(plyr)
library(stringr)
library(ggplot2)

makeTrendPlot = function(res, pid, indic, prop) {

  res = res[[indic]]
  # mean indicator contribution for each algo
  res = subset(res, res$prob == pid)
  res$algo2 = paste(res$algo, res$prop.points, res$indicator, res$crit, sep = "-")
  res$algo2 = str_replace_all(res$algo2, "-NA", "")
  res = ddply(res, "algo2", 
    function(x) colMeans(x[, 7:(ncol(res)-2)]))
  print(res)
  # reshape format of dataset
  reshaped = data.frame (
    algo2 = rep(res$algo2, each = ncol(res) - 2),
    dob = rep(1:(ncol(res) - 2), nrow(res)),
    indic = as.vector(t(as.matrix(res[, -(1:2)])))
  )
  
  p =  ggplot(reshaped, aes(x = dob, y = indic, col = algo2))
  p = p + geom_line(lwd = 1.25) + xlab("Iteration") + ylab(indic)
  p = p + ggtitle(paste("Trend for ", indic, " indicator of ", prop, "-point algorithms on ", pid, sep = ""))
  p
}

pdf("indicator_trends.pdf")
for (pid in c("GOMOP2_2D3M", "GOMOP_2D2M", "GOMOP_2D5M")) {
  print(pid)
  print(makeTrendPlot(res.2d.single, pid, "hv", prop = 1L))
  print(makeTrendPlot(res.2d.quad,   pid, "hv", prop = 4L))
  print(makeTrendPlot(res.2d.single, pid, "r2", prop = 1L))
  print(makeTrendPlot(res.2d.quad,   pid, "r2", prop = 4L))
  print(makeTrendPlot(res.2d.single, pid, "eps", prop = 1L))
  print(makeTrendPlot(res.2d.quad,   pid, "eps", prop = 4L))
}
for (pid in c("GOMOP_5D2M", "GOMOP_5D5M", "dtlz2_5D2M", "dtlz2_5D5M",
  "zdt1_5D2M", "zdt2_5D2M", "zdt3_5D2M")) {
  print(pid)
  print(makeTrendPlot(res.5d.single, pid, "hv", prop = 1L))
  print(makeTrendPlot(res.5d.quad,   pid, "hv", prop = 4L))
  print(makeTrendPlot(res.5d.single, pid, "r2", prop = 1L))
  print(makeTrendPlot(res.5d.quad,   pid, "r2", prop = 4L))
  print(makeTrendPlot(res.5d.single, pid, "eps", prop = 1L))
  print(makeTrendPlot(res.5d.quad,   pid, "eps", prop = 4L))
}
dev.off()

library(ggplot2)
load("bench//mco/mcoBenchResults.RData")
res$algo2 = as.factor(ifelse(res$algo == "nsga2", paste(res$algo, res$generations),
  paste(res$algo, res$prop.points)))

# Calc means
round(tapply(res$hv, list(res$algo2, res$prob), mean), 2)
round(tapply(res$front.size, list(res$algo2, res$prob), mean), 2)
round(tapply(res$r2, list(res$algo2, res$prob), mean), 2)
round(tapply(res$cd.median, list(res$algo2, res$prob), mean), 2)


# Make Plots
pdf("bench/mco/boxplots.pdf")
p <- ggplot(res, aes(prob, hv, fill = algo2))
print(p + geom_boxplot())
p <- ggplot(res, aes(prob, r2, fill = algo2))
print(p + geom_boxplot() + scale_y_log10(limits = c(1e-5, 2e2)))
p <- ggplot(res, aes(prob, cd.median, fill = algo2))
print(p + geom_boxplot() + scale_y_log10())
dev.off()

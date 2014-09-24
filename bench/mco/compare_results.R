load("results.RData")

library(stringr)
library(reshape2)
library(xtable)

prob.ids = unique(aggr$prob.id)

# Tests if x < y[i,] and x > y[i,] for all i. uses a paired wilcoxon test
# Result is a factor for every test "better" "not_worse" "worse".
# E.g. y[i] is better than x
test = function(x, ys) {
  # tests, if x - y < or > 0
  res = factor(levels = c("better", "not worse", "worse"))
  for (i in seq_row(ys)) {
    diff = ys[i, ] - x
    if (wilcox.test(diff, alternativ = "greater")$p.value < 0.05)
      res[i] = "better"
    else if (wilcox.test(diff, alternativ = "less")$p.value < 0.05)
      res[i] = "worse"
    else
      res[i] = "not_worse"
  }
  return(res)
}

compareGroup = function(aggr, expr, ppoints = NA, digits = NULL) {
  d = subset(aggr, expr)
  if (!is.na(ppoints))
    d = subset(d, d$prop.points == ppoints)
  # join algo name and params
  d$algo2 = paste(d$algo, d$budget, d$prop.points, d$indicator, d$crit, sep = "-")
  d$algo2 = str_replace_all(d$algo2, "-NA", "")
  d = dcast(d, d$prob ~ d$algo2, value.var = "hv")
  rownames(d) = d$prob
  d$prob = NULL

  # d = as.matrix(d)
  # d = t(apply(d, 1, function(x) {
    # y = x[-1]
    # j = which.max(y) + 1
    # x[j] = paste(x[j], "*")
    # x
  # }))
  return(as.data.frame(d))
}

tab1 = compareGroup(aggr, aggr$budget %in% c("normal", NA) & aggr$prop.points %in% c(1, NA) & aggr$indicator %in% c("sms", NA) &
  (aggr$algo != "parego" | aggr$crit == "lcb"), digits = 3)
write.table(print(xtable(tab1)), file= "table1.tex")

compareGroup(aggr, (aggr$budget %in% c("normal", NA)) & (aggr$algo == "dib" | aggr$prop.points == 4) & (aggr$indicator %in% c("sms", NA)), digits = 3)

tab.parego = compareGroup(aggr, aggr$algo == "parego", digits = 3)
write.table(print(xtable(tab.parego)), file= "tableParego.tex")

# Parego analyse
# 1) LCB ist ueberall besser, ausser auf einer funktion wo es egal ist
# 2) Multipoint ist entweder besser, oder es ist es stueck nur schlecher / gleich

tab.dib = compareGroup(aggr, aggr$algo == "dib", digits = 3)
write.table(print(xtable(tab.dib)), file= "tableDib.tex")

# DIB
# 1) 1-point liegt sms klar vorne, ist aber nicht sehr viel
# 2) Multipoint ist es nicht ganz klar (?), aber man wuerde wohl auch den SMS nehmen. eps ist teilweise auch ok,
#    wird manchmal aber outperformed

tab.mspot = compareGroup(aggr, aggr$algo == "dib", digits = 3)
write.table(print(xtable(tab.mspot)), file= "tableMspot.tex")




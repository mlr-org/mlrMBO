load("results.RData")

library(stringr)
library(reshape2)

prob.ids = unique(aggr$prob.id)

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

compareGroup(aggr, aggr$budget %in% c("normal", NA) & aggr$prop.points %in% c(1, NA) & aggr$indicator %in% c("sms", NA) &
  (aggr$algo != "parego" | aggr$crit == "lcb"), digits = 3)

compareGroup(aggr, (aggr$budget %in% c("normal", NA)) & (aggr$algo == "dibss" | aggr$prop.points == 4) & (aggr$indicator %in% c("sms", NA)), digits = 3)


compareGroup(aggr, aggr$algo == "parego", digits = 3)

# Parego analyse
# 1) LCB ist ueberall besser, ausser auf einer funktion wo es egal ist
# 2) Multipoint ist entweder besser, oder es ist es stueck nur schlecher / gleich

compareGroup(aggr, aggr$algo == "dib", digits = 3)

# DIB
# 1) 1-point liegt sms klar vorne, ist aber nicht sehr viel
# 2) Multipoint ist es nicht ganz klar (?), aber man wuerde wohl auch den SMS nehmen. eps ist teilweise auch ok,
#    wird manchmal aber outperformed







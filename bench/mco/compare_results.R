load("results.RData")

library(stringr)
library(reshape2)
library(xtable)
library(BBmisc)

# Tests if x < y[, i] and x > y[, i] for all i. uses a paired wilcoxon test
# Result is a factor for every test "better" "not_worse" "worse".
# E.g. y[i] is better than x
# Maximize defines, if smaller or hihger values are better.
test = function(x, ys, maximize = FALSE) {
  # tests, if x - y < or > 0
  if (maximize) {
    x = x * -1
    ys = ys * -1
  }
  res = factor(levels = c("better", "not_worse", "worse"))
  for (i in seq_col(ys)) {
    diff = ys[, i] - x
    if (wilcox.test(diff, alternativ = "less")$p.value < 0.05)
      res[i] = "better"
    else if (wilcox.test(diff, alternativ = "greater")$p.value < 0.05)
      res[i] = "worse"
    else
      res[i] = "not_worse"
  }
  return(res)
}

# Tests all algos versus the ref.algo w.r.t. the given indicator
makeTest = function(d, indicator, ref.algo, maximize) {
  ref.test.res = lapply(levels(d$prob), function(level) {
    tmp = subset(d, d$prob == level)
    tmp.mat = matrix(tmp[, indicator], nrow = 20L)
    ref.algo.ind = which(unique(tmp$algo2) == ref.algo)
    test.res = test(tmp.mat[, ref.algo.ind], tmp.mat[, - ref.algo.ind, drop = FALSE], maximize = maximize)
    names(test.res) = setdiff(unique(tmp$algo2), ref.algo)
    return(test.res)
  })
  names(ref.test.res)  = levels(d$prob)
  return( t(as.matrix(as.data.frame(ref.test.res))))
}

# res: The resultat of our experiment
# expr: expression for subsetting the experiment
# indicator used for comparision
# digits: round to this number of digits
# ref.algo: compare with this ref.algo
# include.baseline: should randomSearch and nsga2 be uncluded?
# label: label for the TeX-Table

compareGroup = function(res, expr, indicator, digits = NULL, ref.algo = NULL,
  include.baseline = FALSE, label = NULL, col.sorting = NULL, caption = NULL) {
  d = subset(res, expr)
  # join algo name and params
  d$algo2 = paste(d$algo, d$budget, d$prop.points, d$indicator, d$crit, sep = "-")
  d$algo2 = str_replace_all(d$algo2, "-NA", "")
  
  maximize = indicator == "hv"
  
  # Test versus ref.algo if one is specified
  if (!is.null(ref.algo)) {
    ref.test.res = makeTest(d, indicator, ref.algo, maximize = maximize)
  }
  
  # Test versus randomSearch and nsga2?
  if (include.baseline) {
    baseline.rs = subset(res, res$algo == "randomSearch" & res$budget == "normal")
    baseline.rs$algo2 = "rs"
    rs.test.res = makeTest(rbind(baseline.rs, d), indicator, "rs", maximize = maximize)
    
    baseline.nsga2 = subset(res, res$algo == "nsga2" & res$budget == "normal")
    baseline.nsga2$algo2 = "nsga2"
    nsga2.test.res = makeTest(rbind(baseline.nsga2, d), indicator, "nsga2", maximize = maximize)
  }
  
  # some conversions
  d = dcast(d, d$prob ~ d$algo2, fun.aggregate = mean, value.var = indicator)
  d.full.precision = d[, -1]
  if (!is.null(digits))
    d[, -1] = round(d[,-1], digits)
  d = as.matrix(d)
  rownames(d) = d[, "d$prob"]
  d = d[, -1]
  
  # the xtable
  xtab = d
  nc = ncol(xtab)
  nr = nrow(xtab)
  
  # mark best algo for each row. bit tricky, since we have to calc the best
  # in d, but mark it in xtab
  for (i in seq_row(xtab)) {
    if (maximize)
      best.ind = which.max(d.full.precision[i, ])
    else
      best.ind = which.min(d.full.precision[i, ])
    xtab[i, best.ind] = paste("\\mathbf{", xtab[i, best.ind], "}", sep= "")
  }
  # start math modus for every cell
  xtab[1:nr, 1:nc] = paste("$", xtab[1:nr, 1:nc], sep = "")
  # mark test versus ref.algo
  if (!is.null(ref.algo)) {
    ref.test.res = ref.test.res[, setdiff(colnames(xtab), ref.algo)]
    ref.algo.ind = which(colnames(xtab) == ref.algo)
    xtab[, -ref.algo.ind][ref.test.res == "better"] = 
      paste(xtab[, -ref.algo.ind][ref.test.res == "better"], "^{++}", sep = "")
    xtab[, -ref.algo.ind][ref.test.res == "not_worse"] = 
      paste(xtab[, -ref.algo.ind][ref.test.res == "not_worse"], "^{+}", sep = "")
  }
  # mark test versus baseline algos and add baseline algos
  if (include.baseline) {
    rs.test.res = rs.test.res[, colnames(xtab)]
    nsga2.test.res = nsga2.test.res[, colnames(xtab)]
    xtab[1:nr, 1:nc] = paste(xtab, "_{", sep = "")
    xtab[rs.test.res == "better"] = 
      paste(xtab[rs.test.res == "better"], "r", sep = "")
    xtab[nsga2.test.res == "better"] = 
      paste(xtab[nsga2.test.res == "better"], "n", sep = "")
    xtab[1:nr, 1:nc] = paste(xtab, "}", sep = "")
  }
  # end math modus for every cell
  xtab[1:nr, 1:nc] = paste(xtab[1:nr, 1:nc], "$", sep = "")
  # add cols with baseline
  if (include.baseline) {
    b = rbind(baseline.rs, baseline.nsga2)
    b = dcast(b, b$prob ~ b$algo2, fun.aggregate = mean, value.var = indicator)
    if (!is.null(digits))
      b[, -1] = round(b[,-1], digits)
    b = as.matrix(b)
    colnames(b) = c("", "nsga2", "rs")
    # exclude for now
    # xtab = cbind(xtab, b[, -1])    
  }
  # some layout polishing
  label = label
  rownames(xtab) = tolower(gsub("_", "-", rownames(xtab)))
  row.names = rownames(xtab)
  nc = nchar(row.names)
  rownames(xtab) = paste(substr(row.names, 1, nc - 3), substr(row.names, nc - 1, nc - 1), sep = "")
  ncols = ncol(xtab)
  align = gsub("", "|", collapse(rep("l", ncols + 1), sep = ""))
  if (!is.null(col.sorting))
    xtab = xtab[, col.sorting]
  xtab = xtable(xtab, caption = caption, label = label, align = align)
  
  # output for the console   
  d = t(apply(d, 1, function(x) {
    if (maximize)
      j = which.max(x)
    else
      j = which.min(x)
   x[j] = paste("*", x[j])
   x
  }))
  d = as.data.frame(d)
  if (include.baseline) {
   d = cbind(d, b[, -1]) 
  }
  if (!is.null(col.sorting))
    d = d[, col.sorting]
  
  return(list(d = as.data.frame(d), xtab = xtab))
}


# Tables for our Paper
# ParEGO Analyse
tab.parego.ei = compareGroup(res = res, expr = res$algo == "parego" & res$crit == "ei", indicator = "r2",
  digits = 3, ref.algo = "parego-1-ei", include.baseline = TRUE, label = "parego.table")
 
tab.parego.lcb = compareGroup(res = res, expr = res$algo == "parego" & res$crit == "lcb", indicator = "r2",
  digits = 3, ref.algo = "parego-1-lcb", include.baseline = TRUE, label = "parego.table")

# DIB - sms Analyse
tab.sms = compareGroup(res = res, expr = res$algo == "dib" & res$indicator %in% c(NA, "sms"),
  indicator = "hv", digits = 3, ref.algo = "dib-1-sms", include.baseline = TRUE, label = "sms.table")

# DIB - eps Analyse
tab.eps = compareGroup(res = res, expr = res$algo == "dib" & res$indicator %in% c(NA, "eps"),
  indicator = "eps", digits = 3, ref.algo = "dib-1-eps", include.baseline = TRUE, label = "eps.table")

# MSPOT Analyse
tab.mspot.mean = compareGroup(res = res, expr = res$algo == "mspot" & res$crit == "mean", indicator = "hv",
  digits = 3, ref.algo = "mspot-1-mean", include.baseline = TRUE, label = "mspot.table")

tab.mspot.ei = compareGroup(res = res, expr = res$algo == "mspot" & res$crit == "ei", indicator = "hv",
  digits = 3, ref.algo = "mspot-1-ei", include.baseline = TRUE, label = "mspot.table")

tab.mspot.lcb = compareGroup(res = res, expr = res$algo == "mspot" & res$crit == "lcb", indicator = "hv",
  digits = 3, ref.algo = "mspot-1-lcb", include.baseline = TRUE, label = "mspot.table")


# best algo tables
expr.single = res$prop.points == 1 & ((res$algo == "dib"& res$indicator == "sms") |
    (res$algo == "parego" & res$crit == "lcb")) 
expr.mult = res$prop.points == 4 & (res$algo == "dib" | (res$algo =="parego" & res$crit == "lcb") |
    (res$algo == "mspot" & res$crit == "lcb"))

all.cmp.eps = compareGroup(res = res, expr = expr.single | expr.mult, indicator = "eps", digits = 3,
  col.sorting = c(1, 5, 2, 3, 4, 6))
all.cmp.r2 = compareGroup(res = res, expr = expr.single | expr.mult, indicator = "r2", digits = 3,
  col.sorting = c(1, 5, 2, 3, 4, 6))
all.cmp.hv = compareGroup(res = res, expr = expr.single | expr.mult, indicator = "hv", digits = 3,
  col.sorting = c(1, 5, 2, 3, 4, 6))

# write tables on disk
write(x = print(tab.parego.ei$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableParegoEi.tex")
write(x = print(tab.paregollcb$xtab, type = "latex",
   sanitize.text.function = function(x){x}), file= "tableParegoLcb.tex")
write(x = print(tab.mspot.mean$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableMspotMean.tex")
write(x = print(tab.mspot.lcb$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableMspotLcb.tex")
write(x = print(tab.mspot.ei$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableMspotEi.tex")
write(x = print(tab.eps$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableEps.tex")
write(x = print(tab.sms$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableSms.tex")
write(x = print(all.cmp.eps$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableBestEps.tex")
write(x = print(all.cmp.r2$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableBestR2.tex")
write(x = print(all.cmp.hv$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableBestHv.tex")

# Tables for our web-page
tab.hv.1 = compareGroup(res = res, expr = res$prop.points == 1L, indicator = "hv",
  digits = 3, include.baseline = TRUE,
  caption = "Singe-point results compared via unary hypervolume indicator.")
tab.hv.4 = compareGroup(res = res, expr = res$prop.points == 4L, indicator = "hv",
  digits = 3, include.baseline = TRUE,
  caption = "Multi-point results compared via unary hypervolume indicator.")
tab.hv.base = compareGroup(res = res, expr = res$algo == "nsga2-ref" | res$budget == "normal",
  indicator = "hv", digits = 3, include.baseline = FALSE,
  caption = "Baseline results compared via unary hypervolume indicator. NSGA2-ref is the mean result of 20 replications nsga2 with 40d population size and 1000 generations.")

tab.r2.1 = compareGroup(res = res, expr = res$prop.points == 1L, indicator = "r2",
  digits = 3, include.baseline = TRUE,
  caption = "Singe-point results compared via unary r2 indicator.")
tab.r2.4 = compareGroup(res = res, expr = res$prop.points == 4L, indicator = "r2",
  digits = 3, include.baseline = TRUE,
  caption = "Multi-point results compared via unary r2 indicator.")
tab.r2.base = compareGroup(res = res, expr = res$algo == "nsga2-ref" | res$budget == "normal",
  indicator = "r2", digits = 3, include.baseline = FALSE,
  caption = "Baseline results compared via unary r2 indicator. Exact front is the mean result of 20 replications nsga2 with 40d population size and 1000 generations.")

tab.eps.1 = compareGroup(res = res, expr = res$prop.points == 1L, indicator = "eps",
  digits = 3, include.baseline = TRUE,
  caption = "Singe-point results compared via epsilon indicator.")
tab.eps.4 = compareGroup(res = res, expr = res$prop.points == 4L, indicator = "eps",
  digits = 3, include.baseline = TRUE,
  caption = "Multi-point results compared via epsilon indicator.")
tab.eps.base = compareGroup(res = res, expr = res$algo == "nsga2-ref" | res$budget == "normal",
  indicator = "eps", digits = 3, include.baseline = FALSE,
  caption = "Baseline results compared via epsilon indicator. Exact front is the mean result of 20 replications nsga2 with 40d population size and 1000 generations.")

write(x = print(tab.hv.1$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "all_hv_single.tex")
write(x = print(tab.hv.4$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "all_hv_multi.tex")
write(x = print(tab.hv.base$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "all_hv_base.tex")

write(x = print(tab.r2.1$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "all_r2_single.tex")
write(x = print(tab.r2.4$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "all_r2_multi.tex")
write(x = print(tab.r2.base$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "all_r2_base.tex")

write(x = print(tab.eps.1$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "all_eps_single.tex")
write(x = print(tab.eps.4$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "all_eps_multi.tex")
write(x = print(tab.eps.base$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "all_eps_base.tex")

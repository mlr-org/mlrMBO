load("results.RData")

library(stringr)
library(reshape2)
library(xtable)
library(BBmisc)

prob.ids = unique(aggr$prob.id)

# Tests if x < y[, i] and x > y[, i] for all i. uses a paired wilcoxon test
# Result is a factor for every test "better" "not_worse" "worse".
# E.g. y[i] is better than x
# Note: Smaller values are better in our case
test = function(x, ys) {
  # tests, if x - y < or > 0
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

# res: The resultat of our experiment
# expr: expression for subsetting the experiment
# indicator used for comparision
# digits: round to this number of digits
# ref.algo: compare with this ref.algo
# include.baseline: should randomSearch and nsga2 be uncluded?
# label: label for the TeX-Table

compareGroup = function(res, expr, indicator, digits = NULL, ref.algo = NULL,
  include.baseline = FALSE, label) {
  d = subset(res, expr)
  # join algo name and params
  d$algo2 = paste(d$algo, d$budget, d$prop.points, d$indicator, d$crit, sep = "-")
  d$algo2 = str_replace_all(d$algo2, "-NA", "")
  
  # Test versus ref.algo if one is specified
  if (!is.null(ref.algo)) {
    ref.test.res = t(sapply(levels(d$prob), function(level) {
      tmp = subset(d, d$prob == level)
      tmp.mat = matrix(tmp[, indicator], nrow = 20L)
      ref.algo.ind = which(unique(tmp$algo2) == ref.algo)
      test.res = test(tmp.mat[, ref.algo.ind], tmp.mat[, - ref.algo.ind, drop = FALSE])
      names(test.res) = setdiff(unique(tmp$algo2), ref.algo)
      return(test.res)
    }))
    if (nrow(ref.test.res) == 1) {
      ref.test.res = t(ref.test.res)
      colnames(ref.test.res) = setdiff(unique(d$algo2), ref.algo)
    }
  }
  
  # Test versus randomSearch and nsga2?
  if (include.baseline) {
    baseline.rs = subset(res, res$algo == "randomSearch" & res$budget == "normal")
    rs.test.res = t(sapply(levels(d$prob), function(level) {
      tmp = subset(d, d$prob == level)
      tmp.mat = matrix(tmp[, indicator], nrow = 20L)
      test.res = test(subset(baseline.rs, baseline.rs$prob == level)[, indicator], tmp.mat)
      names(test.res) = unique(tmp$algo2)
      return(test.res)
    }))
    
    baseline.nsga2 = subset(res, res$algo == "nsga2" & res$budget == "normal")
    nsga2.test.res = t(sapply(levels(d$prob), function(level) {
      tmp = subset(d, d$prob == level)
      tmp.mat = matrix(tmp[, indicator], nrow = 20L)
      test.res = test(subset(baseline.nsga2, baseline.nsga2$prob == level)[, indicator], tmp.mat)
      names(test.res) = unique(tmp$algo2)
      return(test.res)
    }))
  }
  
  # some conversions
  d = dcast(d, d$prob ~ d$algo2, fun.aggregate = mean, value.var = "hv")
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
    best.ind = which.min(d[i, ])
    xtab[i, best.ind] = paste("\\textbf{", xtab[i, best.ind], "}", sep= "")
  }
  # mark test versus ref.algo
  if (!is.null(ref.algo)) {
    ref.test.res = ref.test.res[, setdiff(colnames(xtab), ref.algo)]
    ref.algo.ind = which(colnames(xtab) == ref.algo)
    xtab[, -ref.algo.ind][ref.test.res == "better"] = 
      paste(xtab[, -ref.algo.ind][ref.test.res == "better"], "$^{++}$", sep = "")
    xtab[, -ref.algo.ind][ref.test.res == "not_worse"] = 
      paste(xtab[, -ref.algo.ind][ref.test.res == "not_worse"], "$^{+}$", sep = "")
  }
  # mark test versus baseline algos and add baseline algos
  if (include.baseline) {
    rs.test.res = rs.test.res[, colnames(xtab)]
    nsga2.test.res = nsga2.test.res[, colnames(xtab)]
    xtab[1:nr, 1:nc] = paste(xtab, " $_{", sep = "")
    xtab[rs.test.res == "better"] = 
      paste(xtab[rs.test.res == "better"], "r", sep = "")
    xtab[nsga2.test.res == "better"] = 
      paste(xtab[nsga2.test.res == "better"], "n", sep = "")
    xtab[1:nr, 1:nc] = paste(xtab, "}$", sep = "")
    
    b = subset(res, res$budget == "normal")
    # join algo name and params
    b$algo2 = paste(b$algo, b$budget, b$prop.points, b$indicator, b$crit, sep = "-")
    b$algo2 = str_replace_all(b$algo2, "-NA", "")
    b = dcast(b, b$prob ~ b$algo2, fun.aggregate = mean, value.var = indicator)
    if (!is.null(digits))
      b[, -1] = round(b[,-1], digits)
    b = as.matrix(b)
    colnames(b) = c("", "nsga2", "rs")
    xtab=  cbind(xtab, b[, -1])    
  }
  # caption:
  capt = "TODO"
  label = label
  rownames(xtab) = gsub("_", "-", rownames(xtab))
  xtab = xtable(xtab, caption = capt, label = label)
  
  # output for the console   
  d = t(apply(d, 1, function(x) {
   y = x[-1]
   j = which.min(y) + 1
   x[j] = paste("*", x[j])
   x
  }))
  d = as.data.frame(d)
  
  return(list(d = as.data.frame(d), xtab = xtab))
}

# ParEGO Analyse
tab.parego = compareGroup(res = res, expr = res$algo == "parego", indicator = "r2",
  digits = 3, ref.algo = "parego-1-ei", include.baseline = TRUE, label = "parego.table")
write(x = print(tab.parego$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableParego.tex")
# 1) LCB ist ueberall besser, ausser auf einer funktion wo es egal ist
# 2) Multipoint ist entweder besser, oder es ist es stueck nur schlecher / gleich
 
# DIB - sms Analyse
tab.sms = compareGroup(res = res, expr = res$algo == "dib" & res$indicator %in% c(NA, "sms"),
  indicator = "hv", digits = 3, ref.algo = "dib-1-sms", include.baseline = TRUE, label = "sms.table")
write(x = print(tab.sms$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableSms.tex")

# DIB - eps Analyse
tab.eps = compareGroup(res = res, expr = res$algo == "dib" & res$indicator %in% c(NA, "eps"),
  indicator = "eps", digits = 3, ref.algo = "dib-1-eps", include.baseline = TRUE, label = "eps.table")
write(x = print(tab.eps$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableEps.tex")
# 1) 1-point liegt sms klar vorne, ist aber nicht sehr viel
# 2) Multipoint ist es nicht ganz klar (?), aber man wuerde wohl auch den SMS nehmen. eps ist teilweise auch ok,
#    wird manchmal aber outperformed

# MSPOT Analyse
tab.mspot = compareGroup(res = res, expr = res$algo == "mspot",
  indicator = "hv", digits = 3, ref.algo = "mspot-1-mean", include.baseline = TRUE, label = "mspot.table")
write(x = print(tab.mspot$xtab, type = "latex",
  sanitize.text.function = function(x){x}), file= "tableMspot.tex")

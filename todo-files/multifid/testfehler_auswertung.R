library(BBmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
load("redo_res_all.RData")
source("todo-files/multifid/benchmark/giveMe.R")
file.res = load2("../plots/bJ_classification_2015_0403_1309/CV_compare.RData", parts = c("all.res", "tasks"))
res.clean = res[!is.na(res)]
resample.results.job.id = extractSubList(res.clean, c("job.id"), simplify = TRUE)
resample.results = data.frame(
  task = extractSubList(res.clean, c("resample.result", "task.id"), simplify = TRUE),
  job.id = resample.results.job.id,
  learner.id = sapply(resample.results.job.id, function(i) file.res$all.res[[as.character(i)]]$learner.id),
  timecut = extractSubList(res.clean, c("time.cut"), simplify = TRUE),
  aggr = convertListOfRowsToDataFrame(extractSubList(res.clean, c("resample.result", "aggr"), simplify = FALSE)))
resample.results$learner.id = substr(resample.results.learner.id, start = 13, stop = 999)
g = ggplot(resample.results, aes(y=aggr.mmce.test.mean, x=as.factor(timecut), fill=learner.id))
g = g + geom_boxplot() + facet_wrap(~task, scales = "free")
g = g + scale_x_discrete("Algorithm", drop=FALSE) + scale_y_continuous("mmce") + theme_bw()
color_values =  c("#1BE800", "#246616", "#A2E58C", "#7F0000", "#FF3A39", "#0056CF", "#4C94FF", "#ff69b4", "#551a8b")
g = g + scale_fill_manual(values = color_values, drop = FALSE)
g

head(resample.results)

results.grouped = resample.results %>% dropNamed("job.id") %>% group_by(task, timecut, learner.id)
results.ranked = results.grouped %>% summarise_each(funs(median, mean)) %>% mutate_each(funs(rank), matches("aggr."))
ranked.average = results.ranked %>% group_by(timecut, learner.id) %>% summarise_each(funs(mean), matches("aggr.")) %>% select(timecut, learner.id, mmce_average_rank = aggr.mmce.test.mean_mean)
library(xtable)
print(xtable(ranked.average), type = "HTML")
print(xtable(dcast(ranked.average, learner.id~timecut)), type = "HTML")

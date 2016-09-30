devtools::load_all("~/gits/mlrMBO/")
#dir = "~/lido_nobackup/synRambo_5d_m8_long/mlrMBO/job_391"
setwd(dir)
opt.problem = readRDS(file = "/tmp/RtmpRqrb99/mbo_asyn/asyn/opt_problem.rds")
ctrl = getOptProblemControl(opt.problem)

node.prefix = ""

## propose a further point
opt.state = readDirectoryToOptState(opt.problem)
models = getOptStateModels(opt.state)
x.des = generateDesign(n = 100, par.set = getOptProblemParSet(opt.problem))
real.y = apply(x.des, 1, function(x) sum(x^2))
predicted.y = predict(models$models[[1]], newdata = x.des)$data$response
plot(predicted.y~real.y)
tail(as.data.frame(getOptStateOptPath(opt.state)))

prop = proposePoints.OptState(opt.state)
prop$crit.components$mean
prop$scheduled.on = 2
prop$eval.state = "proposed"
proposal.file = writeThingToDirectory(opt.problem, prop, sprintf("%sprop_", node.prefix))

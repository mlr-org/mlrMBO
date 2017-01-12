# context("multifid")

# test_that("basic multifid works", {
#   skip_on_cran() #we skip because it is not exported

#   f = makeSingleObjectiveFunction(
#     fn = function(x) {
#       lvl.par.val = x$.multifid.lvl
#       x = x$x
#       3 - lvl.par.val + 0.5 * x + rnorm(1)
#     },
#     par.set = makeParamSet(
#       makeNumericParam("x", lower = 0, upper = 10),
#       makeIntegerParam(".multifid.lvl", lower = 1L, upper = 3L)
#     ),
#     has.simple.signature = FALSE, noisy = TRUE, global.opt.value = -2
#   )

#   control = makeMBOControl()
#   control = setMBOControlTermination(control, iters = 5L)

#   control = setMBOControlInfill(control = control,
#                                 crit = ei,
#                                 opt = "focussearch",
#                                 opt.restarts = 1L,
#                                 opt.focussearch.maxit = 1L,
#                                 opt.focussearch.points = 10L,
#                                 filter.proposed.points = TRUE,
#                                 filter.proposed.points.tol = 0.01
#   )
#   control = mlrMBO:::setMBOControlMultiFid(control = control,
#                                   param = "dw.perc",
#                                   costs = 1:3,
#                                   lvls = c(0.1, 0.5, 1),
#                                   cor.grid.points = 40L)

#   surrogat.learner = makeLearner("regr.km", predict.type = "se")
#   set.seed(1)
#   des = generateTestDesign(10L, getParamSet(f), fun = lhs::maximinLHS)
#   set.seed(1)
#   result = mbo(f, des, learner = surrogat.learner, control = control)
#   expect_output(print(result), "Recommended parameters")
#   #this is not realy a hard threashold
#   expect_lt(result$y, 0.25)

#   opdf = as.data.frame(result$opt.path)
#   expect_subset(c("x",".multifid.lvl","y","ei","se","mean","mf.ei.last","mf.se","mf.alpha1","mf.alpha2","mf.alpha3","mf.sd"), colnames(opdf))
#   expect_data_frame(opdf[11:15, c("ei","se","mean","mf.ei.last","mf.se","mf.alpha1","mf.alpha2","mf.alpha3","mf.sd")], any.missing = FALSE)

#   # remove cots so time.model will be estimated
#   control$multifid.costs = NULL
#   f.delay = makeSingleObjectiveFunction(
#     fn = function(x) {
#       Sys.sleep(0.1 + x$.multifid.lvl/3)
#       f(x)
#     },
#     par.set = makeParamSet(
#       makeNumericParam("x", lower = 0, upper = 10),
#       makeIntegerParam(".multifid.lvl", lower = 1L, upper = 3L)
#     ),
#     has.simple.signature = FALSE
#   )
#   set.seed(1)
#   result.time = mbo(f.delay, des, learner = surrogat.learner, control = control)
#   #this is not realy a hard threashold
#   expect_lt(result.time$y, 1)
#   # FIXME: this is pretty hard and migh fail?
#   # expect_equal(as.data.frame(result.time$opt.path)$.multifid.lvl, as.data.frame(result$opt.path)$.multifid.lvl)
# })

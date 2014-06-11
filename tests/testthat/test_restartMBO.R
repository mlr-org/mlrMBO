context("restart MBO")

test_that("restart MBO", {
  f = function(x) {
    i <<- i + 1
    if (i > 12)
      if (rbinom(1, 1, 0.5)) 
        stop ("foo")
    sum(x^2)
  }
  environment(f) <- new.env()
  environment(f)$i <- 0
  
  f = makeMBOFunction(f)
  ps = makeParamSet(
    makeNumericParam("x1", lower = -2, upper = 1),
    makeNumericParam("x2", lower = -1, upper = 2)
  )
  
  # First test sombo
  learner = makeLearner("regr.randomForest")
  save.file = tempfile(fileext  ="mboData")
  ctrl = makeMBOControl(iters = 15, infill.opt.focussearch.points = 100, save.on.disk.at = 0:14,
    save.file.path = save.file, init.design.points = 10L, suppress.eval.errors = TRUE)
  try(or <- mbo(f, ps, learner = learner, control = ctrl, show.info = FALSE), silent = TRUE)
  for(i in 1:100) {
    try(or <- restartSavedMBO(save.file), silent = TRUE)
    if(exists("or"))
      break
  }
  expect_equal(getOptPathLength(or$opt.path), 25)
  
  # Now test parEGO
  f = function(x) {
    i <<- i + 1
    if (i > 12)
      if (rbinom(1, 1, 0.5)) 
        stop ("foo")
    c(sum(x^2), prod(x^2))
  }
  environment(f) <- new.env()
  environment(f)$i <- 0
  
  f = makeMBOFunction(f)
  ctrl = makeMBOControl(iters = 15, infill.opt.focussearch.points = 100, save.on.disk.at = 0:14,
    save.file.path = save.file, init.design.points = 10L, number.of.targets = 2)
  rm(or)
  try(or <- mbo(f, ps, learner = learner, control = ctrl, show.info = FALSE), silent = TRUE)
  for(i in 1:100) {
    try(or <- restartSavedMBO(save.file), silent = TRUE)
    if(exists("or"))
      break
  }
  expect_equal(getOptPathLength(or$opt.path), 25)
})

context("mbo identification of final points")

test_that("final OCBA identification works with time constraint", {
  ps = makeNumericParamSet("x", 1L, -3, 3)
  fun = makeSingleObjectiveFunction(
    fn = function(x) (x + 0.5)^2 + 5 * sin(3 * (x + 0.5)) + rnorm(1, sd = 2),
    par.set = ps, 
    noisy = TRUE
  )

  design = generateDesign(n = 3L, par.set = ps)

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, time.budget = 2L, identification.time.budget = 2L)
  ctrl = setMBOControlNoisy(ctrl, method = "ocba", ocba.initial = 2L, ocba.budget = 0L, identification.pcs = 1)

  # check if time works correctly 
  time = Sys.time()
  or = mbo(fun, design = design, control = ctrl, show.info = TRUE)
  timeend = as.numeric(difftime(Sys.time(), time), units = "secs")

  # check that time budget is correct 
  expect_true(timeend <= ctrl$time.budget + ctrl$identification.time.budget + 5)

  # check if each point is at least evaluated twice
  ds = as.data.table(or$opt.path)
  ds = ds[, runs := .N, by = "x"]
  expect_true(all(ds$runs >= 2L))
})

test_that("final OCBA identification works with pcs constraint", {
  ps = makeNumericParamSet("x", 1L, -3, 3)
  fun = makeSingleObjectiveFunction(
    fn = function(x) (x + 0.5)^2 + 5 * sin(3 * (x + 0.5)) + rnorm(1, sd = 2),
    par.set = ps, 
    noisy = TRUE
  )

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, time.budget = 2L, identification.time.budget = 5L)
  ctrl = setMBOControlNoisy(ctrl, method = "ocba", ocba.initial = 2L, ocba.budget = 0L, identification.pcs = 0.01)

  or = mbo(fun, control = ctrl, show.info = TRUE)

  # check if time works correctly 
  pcs = mlrMBO:::calculatePCS(or$final.opt.state)
  expect_true(pcs > ctrl$noisy.identification.pcs)
})

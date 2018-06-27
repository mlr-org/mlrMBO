context("mbo intensification")

test_that("mbo works with incumbent strategy", {
  ps = makeNumericParamSet("x", 1L, -3, 3)
  fun = smoof::makeSingleObjectiveFunction(
	  fn = function(x) (x + 0.5)^2 + 5 * sin(3 * (x + 0.5)) + rnorm(1, sd = 2),
	  par.set = ps, 
	  noisy = TRUE
	)

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlNoisy(ctrl, method = "incumbent", incumbent.nchallengers = 2L)
  or = mbo(fun, control = ctrl)
  opdf = as.data.frame(or$opt.path)

  # incumbent and proposed point are evaluated once in each iteration
  expect_true(all(table(opdf$prop.type)[c("incumbent", "infill_cb")] == 5L))
  # exactly 3 challengers are evaluated in each iteration (1 new point + 2 old points)  
  expect_true(all(setDT(opdf)[opdf$prop.type == "challenger", .(length(unique(x))), by = "dob"]$V1 == 3L))

  # incumbent of iteration i + 1 should be the best point of iteration i
  for (i in 1:5L) {
    curropdf = opdf[opdf$dob < i, .(ymean = mean(y)), by = x]
    xinc = opdf[opdf$prop.type == "incumbent", "x"][i]
    expect_true(curropdf[which.min(curropdf$ymean), ]$x == xinc)
  }
})


test_that("mbo works with ocba replication strategy", {
  ps = makeNumericParamSet("x", 1L, -3, 3)
  fun = makeSingleObjectiveFunction(
	  fn = function(x) (x + 0.5)^2 + 5 * sin(3 * (x + 0.5)) + rnorm(1, sd = 2),
	  par.set = ps, 
	  noisy = TRUE
	)

	ctrl = makeMBOControl()
	ctrl = setMBOControlTermination(ctrl, iters = 5L)
	ctrl = setMBOControlNoisy(ctrl, method = "ocba", ocba.budget = 12L, ocba.initial = 3L)

	or = mbo(fun, control = ctrl)
	opdf = as.data.frame(or$opt.path)

	# in each iteration we do 12L replications
	opdfs = setDT(opdf)[, .(N = sum(prop.type == "OCBA")), by = dob]
	expect_true(all(opdfs[opdfs$dob > 0, "N"] == 12L))

	# each point is evaluated 3L times at least
	opdfs = setDT(opdf)[, .N, by = "x"]
	expect_true(all(opdfs$N >= 3L))

})


test_that("mbo with replication works in multidimensional case", {
  f = makeAckleyFunction(5L)

  fun = makeSingleObjectiveFunction(
  	fn = function(x) f(x) + rnorm(1, sd = 2), 
  	par.set = getParamSet(f)
  	)

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 5L)
  ctrl = setMBOControlNoisy(ctrl, method = "ocba", ocba.budget = 12L, ocba.initial = 3L)

  or = mbo(fun, control = ctrl)
  opdf = as.data.frame(or$opt.path)

  # in each iteration we do 12L replications
  opdfs = setDT(opdf)[, .(N = sum(prop.type == "OCBA")), by = dob]
  expect_true(all(opdfs[opdfs$dob > 0, "N"] == 12L))

  # each point is evaluated 3L times at least
  opdfs = setDT(opdf)[, .N, by = eval(paste("x", 1:5, sep = ""))]
  expect_true(all(opdfs$N >= 3L))
})

test_that("per instance aggregation works for different functions", {
  f = makeAckleyFunction(1L)
  fun = makeSingleObjectiveFunction(
  	fn = function(x) f(x) + rnorm(1, sd = 2), 
  	par.set = getParamSet(f)
  )

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 3L)
  ctrl = setMBOControlNoisy(ctrl, method = "incumbent", instance.aggregation = median)

  or = mbo(fun, control = ctrl)
  opdf = as.data.frame(or$opt.path)

  expect_true(all(length(or$models[[1]]$subset) == length(unique(opdf$x))))
})

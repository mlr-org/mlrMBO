context("exampleRun")

test_that("exampleRun", {
	library(ggplot2)
	
	n.iters = 2L

	doRun = function(obj.fn, par.set, predict.type, crit) {
		learner = makeLearner("regr.km", predict.type = predict.type)
		control = makeMBOControl(init.design.points = 10, iters = n.iters)
		control = setMBOControlInfill(control, crit = crit, opt = "focussearch", opt.focussearch.points = 10)
		run = exampleRun(makeMBOFunction(obj.fn), global.opt = 0L, par.set, learner, control = control)
		return(autoplot(run, pause = FALSE, show.info = FALSE))
	}

	obj.fn = function(x) sum(x*x)
	par.set = makeParamSet(
		makeNumericParam("x", lower = -2, upper = 2)
	)

	checkPlotList = function(plot.list) {
		expect_is(plot.list, "list")
		expect_equal(length(plot.list), n.iters)
		lapply(plot.list, function(pl.sublist) {
			expect_is(pl.sublist, "list")
			lapply(pl.sublist, function(pl) {
				expect_is(pl, "gg")
				expect_is(pl, "ggplot")
			})
		})
	}

	# without se
	plot.list = doRun(obj.fn, par.set, "response", "mean")
	checkPlotList(plot.list)

	# with se
	plot.list = doRun(obj.fn, par.set, "se", "ei")
	checkPlotList(plot.list)
})

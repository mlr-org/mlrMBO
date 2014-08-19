context("exampleRun")

test_that("exampleRun", {
	library(ggplot2)
	library(soobench)

	n.iters = 2L

	doRun = function(obj.fn, par.set, predict.type, crit, learner = "regr.km", has.simple.signature = TRUE) {
		learner = makeLearner(learner, predict.type = predict.type)
		control = makeMBOControl(init.design.points = 10, iters = n.iters)
		control = setMBOControlInfill(control, crit = crit, opt = "focussearch", opt.focussearch.points = 10)
		if (has.simple.signature)
			obj.fn = makeMBOFunction(obj.fn)
		run = exampleRun(obj.fn, global.opt = 0L, par.set, learner, control = control)
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
				# sometimes for example the 'se' plot is NA, if learner does not support standard error estimation
				if (!is.na(pl)) {
					expect_is(pl, "gg")
					expect_is(pl, "ggplot")	
				}
			})
		})
	}

	# without se
	plot.list = doRun(obj.fn, par.set, "response", "mean")
	checkPlotList(plot.list)

	# with se
	plot.list = doRun(obj.fn, par.set, "se", "ei")
	checkPlotList(plot.list)

	obj.fn = function(x) {
	  	if (x$foo == "a")
	    	sum(x$x^2)
	  	else if (x$foo == "b")
	    	sum(x$x^2) + 10
	  	else
	    	sum(x$x^2) - 10
	}

	par.set = makeParamSet(
	  	makeDiscreteParam("foo", values = letters[1:3]),
	  	makeNumericVectorParam("x", len = 1, lower = -2, upper = 3)
	)

	plot.list = doRun(obj.fn, par.set, "se", "ei", "regr.randomForest", has.simple.signature = FALSE)
	checkPlotList(plot.list)
})

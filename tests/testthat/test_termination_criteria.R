context("termination criteria")

test_that("termination criteria works", {

  f = testf.fsphere.1d
  f.slow = testf.fsphere.1d.slow
  f.max = convertToMaximization(f)
  design = testd.fsphere.1d
  design.max = design
  design$y = apply(design, 1, f)
  design.max$y = apply(design.max, 1, f.max)
  learner = NULL

  term.sets = list(
    list(arg = list(iters = 3L), state = "term.iter"),
    list(arg = list(time.budget = 3L), state = "term.time"),
    list(arg = list(exec.time.budget = 2), state = "term.exectime"),
    list(arg = list(target.fun.value = min(design$y) / 2, iters = 30L), state = "term.yval"),
    list(arg = list(max.evals = nrow(design) + 2L), state = "term.feval")
  )

  for (term.set in term.sets) {
    ctrl = makeMBOControl()
    ctrl = do.call(setMBOControlTermination, c(list(control = ctrl), term.set$arg))

    if (term.set$state == "term.exectime") {
      this.f = f.slow
    } else {
      this.f = f
    }

    or = mbo(this.f, design = design, control = ctrl, learner = learner)
    expect_equal(or$final.state, term.set$state)

    if (term.set$state == "term.iter") {
      expect_equal(getOptPathLength(or$opt.path), term.set$arg$iters + nrow(design))
    }
    if (term.set$state == "term.yval") {
      expect_lt(or$y, term.set$arg$target.fun.value)
      term.set$arg$target.fun.value = term.set$arg$target.fun.value * (-1)
      ctrl = do.call(setMBOControlTermination, c(list(control = ctrl), term.set$arg))
      or.max = mbo(f.max, design = design.max, control = ctrl, learner = learner)
      expect_lt(abs(getOptPathLength(or$opt.path)-getOptPathLength(or.max$opt.path)), 2)
    }
    if (term.set$state == "term.feval") {
      expect_equal(getOptPathLength(or$opt.path), term.set$arg$max.evals)
    }
  }
})

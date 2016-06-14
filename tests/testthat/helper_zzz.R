if (!is.na(Sys.getenv("NOT_CRAN", unset = NA))) {
  set.seed(1)
} else {
  seed.val = sample(1:100, size = 1)
  set.seed(seed.val)
  catf("Run test with seed: %i", seed.val)
}

configureMlr(show.learner.output = FALSE)
options(mlrMBO.show.info = FALSE, mlrMBO.debug.mode = TRUE)

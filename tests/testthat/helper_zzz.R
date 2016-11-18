# We want to run on cran with a fixed seed (1) so that tests will not fail stochastically
# For local testing + Travis we want to detect and be able reproduce these so we sample a seed
if (!is.na(Sys.getenv("NOT_CRAN", unset = NA))) {
  set.seed(1)
} else {
  seed.val = sample(1:10, size = 1)
  set.seed(seed.val)
  catf("Run test with seed: %i", seed.val)
}

configureMlr(show.learner.output = FALSE)
options(mlrMBO.show.info = FALSE, mlrMBO.debug.mode = TRUE)

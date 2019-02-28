# We want to run on cran with a fixed seed (1) so that tests will not fail stochastically
# For local testing + Travis we want to detect and be able reproduce these so we sample a seed
if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true")) {
  seed.val = sample(1:100, size = 1)
  set.seed(seed.val)
  # Only on Travis logs we want to see the used seed
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    catf("Run test with seed: %i", seed.val)
  }
} else {
  set.seed(1)
}

configureMlr(show.learner.output = FALSE)
options(mlrMBO.show.info = FALSE, mlrMBO.debug.mode = TRUE)

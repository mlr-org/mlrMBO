if (!identical(Sys.getenv("TRAVIS"), "true"))
  set.seed(1)
configureMlr(show.learner.output = FALSE)
options(mlrMBO.show.info = FALSE, mlrMBO.debug.mode = TRUE)

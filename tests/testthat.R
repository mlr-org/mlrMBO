library(testthat)

# the unit tests take pretty long, that can be a problem on WB and cran (and maybe annoying locally)
# so we run all tests only on travis and if a certain user env var is set
if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true") || identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("mlrMBO")
} else {
  test_check("mlrMBO", filter = "((mbo_rf)|(mbo_km))")
}

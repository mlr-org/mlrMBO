library(mlrMBO)

# check all demo examples here

if (any(c("R_EXPENSIVE_TEST_OK") %in% names(Sys.getenv()))) {
# if (any(c("TRAVIS", "R_EXPENSIVE_TEST_OK") %in% names(Sys.getenv()))) {
  library(BBmisc)

  dir = file.path(path.package("mlrMBO"), file.path("examples"))
  files = list.files(dir, pattern = ".R", full.names = TRUE)
  for (f in files) {
    messagef("Checking example '%s'", f)
    source(f)
  }
}

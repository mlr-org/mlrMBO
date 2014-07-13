Sys.sleep(0.000000)
options(BatchJobs.on.slave = TRUE, BatchJobs.resources.path = '/home/schork/mlrMBO/bench/mco/mco_bench-files/resources/resources_1405265848.RData')
library(BatchJobs)
res = BatchJobs:::doJob(
	reg = loadRegistry('/home/schork/mlrMBO/bench/mco/mco_bench-files'),
	ids = c(9L,17L,53L,107L,123L,129L,139L,163L,184L,194L,304L,307L,314L,322L,332L,412L,464L,475L,493L,675L,699L,730L,732L,758L,830L,833L,837L,896L,919L),
	multiple.result.files = FALSE,
	disable.mail = FALSE,
	first = 15L,
	last = 7L,
	array.id = NA)
BatchJobs:::setOnSlave(FALSE)
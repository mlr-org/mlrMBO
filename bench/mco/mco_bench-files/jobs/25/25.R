Sys.sleep(0.000000)
options(BatchJobs.on.slave = TRUE, BatchJobs.resources.path = '/home/schork/mlrMBO/bench/mco/mco_bench-files/resources/resources_1405265848.RData')
library(BatchJobs)
res = BatchJobs:::doJob(
	reg = loadRegistry('/home/schork/mlrMBO/bench/mco/mco_bench-files'),
	ids = c(25L,39L,82L,150L,209L,237L,275L,306L,361L,426L,431L,436L,491L,546L,571L,573L,600L,643L,685L,733L,760L,764L,783L,789L,836L,873L,891L,932L,956L),
	multiple.result.files = FALSE,
	disable.mail = FALSE,
	first = 15L,
	last = 7L,
	array.id = NA)
BatchJobs:::setOnSlave(FALSE)
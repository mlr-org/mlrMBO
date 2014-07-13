Sys.sleep(0.000000)
options(BatchJobs.on.slave = TRUE, BatchJobs.resources.path = '/home/schork/mlrMBO/bench/mco/mco_bench-files/resources/resources_1405265848.RData')
library(BatchJobs)
res = BatchJobs:::doJob(
	reg = loadRegistry('/home/schork/mlrMBO/bench/mco/mco_bench-files'),
	ids = c(77L,84L,98L,118L,174L,186L,205L,243L,278L,396L,415L,450L,492L,516L,532L,547L,548L,581L,582L,635L,682L,742L,795L,803L,805L,832L,835L,854L,884L,958L),
	multiple.result.files = FALSE,
	disable.mail = FALSE,
	first = 15L,
	last = 7L,
	array.id = NA)
BatchJobs:::setOnSlave(FALSE)
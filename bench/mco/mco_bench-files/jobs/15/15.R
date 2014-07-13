Sys.sleep(0.000000)
options(BatchJobs.on.slave = TRUE, BatchJobs.resources.path = '/home/schork/mlrMBO/bench/mco/mco_bench-files/resources/resources_1405265848.RData')
library(BatchJobs)
res = BatchJobs:::doJob(
	reg = loadRegistry('/home/schork/mlrMBO/bench/mco/mco_bench-files'),
	ids = c(15L,42L,131L,228L,249L,258L,403L,406L,420L,433L,439L,452L,454L,459L,511L,549L,566L,587L,593L,621L,688L,698L,748L,881L,882L,886L,889L,906L,931L),
	multiple.result.files = FALSE,
	disable.mail = FALSE,
	first = 15L,
	last = 7L,
	array.id = NA)
BatchJobs:::setOnSlave(FALSE)
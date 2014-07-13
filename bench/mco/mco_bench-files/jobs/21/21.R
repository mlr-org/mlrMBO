Sys.sleep(0.000000)
options(BatchJobs.on.slave = TRUE, BatchJobs.resources.path = '/home/schork/mlrMBO/bench/mco/mco_bench-files/resources/resources_1405265848.RData')
library(BatchJobs)
res = BatchJobs:::doJob(
	reg = loadRegistry('/home/schork/mlrMBO/bench/mco/mco_bench-files'),
	ids = c(21L,30L,75L,127L,149L,167L,183L,241L,254L,264L,371L,428L,472L,520L,536L,540L,567L,578L,583L,599L,614L,629L,644L,687L,825L,855L,883L,924L,955L),
	multiple.result.files = FALSE,
	disable.mail = FALSE,
	first = 15L,
	last = 7L,
	array.id = NA)
BatchJobs:::setOnSlave(FALSE)
# in:
#	opt.paths: list of opt.paths
# 	add.const.columns: list(), name gives the column name, values give a constant value for each data.frame
listOfOptPathsToDf = function(opt.paths, add.const.columns) {
	assertList(opt.paths)
	assertList(add.const.columns)
	assertNamed(add.const.columns)
	opt.path.dfs = lapply(opt.paths, as.data.frame)
	opt.path.dfs = lapply(names(add.const.columns), function(cname) {
		lapply(seq_along(opt.path.dfs), function(i) {
			opt.path.dfs[[i]][,cname] = add.const.columns[[cname]][i]
			opt.path.dfs 
		})
		invisible(NULL)
	})
	do.call(rbind, opt.path.dfs)
}
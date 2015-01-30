# 0. Load packages and seed
library("devtools")
load_all()
library("BBmisc")
library("checkmate")
library("mlr")
library("ggplot2")
library("reshape2")
library("plyr")
library("gridExtra")

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

checkOptPath = function(op, forced.columns = NULL) {
	assertClass(op, classes = "OptPath")
	assertCharacter(op$y.names, len = 1)
	op.df = as.data.frame(op)
	if (!is.null(forced.columns)) {
		assertSubset(forced.columns, colnames(op.df))
	}
	list(
		x.pars = getParamIds(op$par.set),
		y.names = op$y.names,
		op.df = op.df
		)

}

checkOptPathList = function(ops, forced.columns = character()) {
	assertList(ops)
	assertCharacter(forced.columns)
	x.pars = getParamIds(ops[[1]]$par.set)
	y.names = ops[[1]]$y.names
	if (length(forced.columns)) {
		forced.columns = as.data.frame(ops[[1]])
	} else {
		forced.columns = union(forced.columns, c(x.pars, y.names))
	}
	op.dfs = extractSubList(
		lapply(ops, checkOptPath, forced.columns = forced.columns),
		"op.df", simplify = FALSE)
	list(
		x.pars = x.pars,
		y.names = y.names,
		op.dfs = op.dfs
		)
}
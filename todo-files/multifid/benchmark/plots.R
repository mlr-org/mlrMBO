genPlotCompareMbos = function(opt.path.grids, opt.path.cheap, opt.path.expensive) {
	assertList(opt.path.grids)
	ops.res = checkOptPathList(opt.path.grids)
	x.pars = ops.res$x.pars
	y.names = ops.res$y.names
	forced.columns = c(x.pars, y.names)
	df.cheap = checkOptPath(opt.path.cheap, forced.columns = forced.columns)$op.df
	df.expensive = checkOptPath(opt.path.expensive, forced.columns = forced.columns)$op.df
	df.grid = do.call(rbind, ops.res$op.dfs)
	df.mbo = rbind(
		cbind(df.cheap, .multifid.lvl = 1), 
		cbind(df.expensive, .multifid.lvl = length(opt.path.grids))
		)
	g = ggplot(
		df.mbo, 
		aes_string(x = x.pars, y = y.names, color = "dob", shape = "as.factor(.multifid.lvl)"))
	g = g + geom_point(size = 4, alpha = 0.6)
	g = g + geom_line(data = df.grid, alpha = 0.5, lty = 2, color = "black", mapping = aes(group = .multifid.lvl))
	return(g)
}

# genPlotMultiFidGrid = function(opt.path.grids)
genPlotSteps = function(multi.opt.path) {
	op.res = checkOptPath(multi.opt.path, forced.columns = c(".multifid.lvl"))
	df = op.res$op.df
	df = df[df$dob > 0, ]
	g = ggplot(df, aes(y = .multifid.lvl, x = dob))
    g = g + geom_line() + geom_point(aes_string(size = op.res$y.names)) 
    return(g)
}

genPlotOptPoints = function(opt.path.grids, opt.paths, final.points) {
	assertDataFrame(final.points)
	assertSubset(c("x", "y", "method"), colnames(final.points))
	op.grid.res = checkOptPathList(opt.path.grids)
	x.pars = op.grid.res$x.pars
	y.names = op.grid.res$y.names
	df.grid = do.call(rbind, op.grid.res$op.dfs)
	g = ggplot()
	g = g + geom_line(
		data = df.grid, 
		alpha = 0.5, 
		mapping = aes_string(x = x.pars, y = y.names, group = ".multifid.lvl"))
	g = g + geom_vline(
		data = final.points,
		mapping = aes(xintercept = x, color = method))
	g = g + geom_hline(
		data = final.points,
		mapping = aes(yintercept = y, color = method))
	return(g)
}


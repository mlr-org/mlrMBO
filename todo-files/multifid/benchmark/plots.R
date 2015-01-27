genPlotCompareMbos = function(opt.path.grids, opt.path.cheap, opt.path.expensive) {
	df.cheap = as.data.frame(opt.path.cheap)
	df.expensive = as.data.frame(opt.path.expensive)
	df.grid = do.call(rbind, do.call(as.data.frame, opt.path.grids))
	df.mbo = rbind(
		cbind(df.cheap, .multifid.lvl = 1), 
		cbind(op2, .multifid.lvl = length(opt.path.grids))
		)
	g = ggplot(
		df.mbo, 
		aes_string(x = getParamIds(e.par.set), y = "y", color = "dob", shape = "as.factor(.multifid.lvl)"))
	g = g + geom_point(size = 4, alpha = 0.6)
	g = g + geom_line(data = df.grid.1, alpha = 0.5, lty = 2, color = "black", mapping = aes(group = .multifid.lvl))
	return(g)
}

# genPlotMultiFidGrid = function(opt.path.grids)
genPlotSteps = function(multi.opt.path) {
	df = as.data.frame(multi.opt.path)
	df = df[df$dob > 0, ]
	g = ggplot(df, aes(y = .multifid.lvl, x = dob))
    g = g + geom_line() + geom_point(aes(size = y)) 
    return(g)
}

genPlotOptPoints = function(opt.path.grids, opt.paths) {
	
}


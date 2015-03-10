# 0. Load packages and seed
library("BBmisc")
library("checkmate")
library("mlr")
library("ggplot2")
library("reshape2")
library("plyr")
library("gridExtra")
library("compiler")
library("SparseM")
library("Matrix")
library("e1071")



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

OpsAddByIter = function(ops, best.col = NULL, init.design.points = NULL) {
  op.dfs = lapply(ops, as.data.frame)
  op.dfs = lapply(seq_along(op.dfs), function(i) {
    df = op.dfs[[i]]
    df[,"iter"] = i
    df 
  })
  if (!is.null(best.col) || !is.null(init.design.points)) {
    op.dfs = lapply(op.dfs, function(df) {
      if (!is.null(best.col)) {
        assertSubset(best.col, colnames(df))
        df = cbind(df, trailingMin(df[, c("dob",best.col), drop = FALSE]))
      }
      if (!is.null(init.design.points)) {
        df$phase = ifelse(df$dob <= init.design.points, "init", "algo")
      }
      df$exec.time.cum = cumsum(df$exec.time)
      return(df)
    })
  }
}

trailingMin.uncomp = function(x) {
  stopifnot(colnames(x)[1] == "dob")
  min.x = Inf
  for(i in seq_row(x)){
    if (x[i,2] < min.x) {
      min.x = x[i,2]
    } else {
      x[i,2] = min.x
      x[i,1] = x[i-1,1]
    }
  }
  colnames(x) = paste0(colnames(x)[2],c(".best.index", ".best"))
  x
}

trailingMin = cmpfun(trailingMin.uncomp)

libsvm.read = function(file) {
  dataset = read.matrix.csr(file)
  colNames = sapply( (1:(dim(dataset$x)[2])), FUN = function(x) { paste("X",x, sep = "") })
  dataframe = as.data.frame(as.matrix(dataset$x))
  colnames(dataframe) = colNames
  dataframe$Y = dataset$y
  dataframe
}

getOptPathColAtTimes = function(op, times) {
  if (!is.data.frame(op))
    op = as.data.frame(op)
  assertNumeric(op$exec.time)
  op$exec.time.cum = cumsum(op$exec.time)
  times = times[times>=getFirst(op$exec.time.cum)]
  res = adply(times, 1, function(t) getOptPathColAtTime(op, t))
  res$time = times
  res$finished = c(FALSE, head(max(res$exec.time.cum) == res$exec.time.cum, -1)) #the last opt.path element doesnt count as finished
  res
}

getOptPathColAtTime = function(op.df, time) {
  col = op.df[which.last(op.df$exec.time.cum<=time), ]
  col
}

Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


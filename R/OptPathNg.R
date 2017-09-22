## mlr-org/mlrng/attic/OptPath.R - 18.09.2017

#' @importFrom R6 R6Class
#' @import data.table
OptPathNg = R6Class(c("OptPathNg", "OptPath"),
  public = list(
    initialize = function(par.set, y.names = "y", minimize = TRUE) {
      x.names = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
      self$full.data = data.table(
        dob = integer(0L),
        eol = integer(0L),
        msg = character(0L),
        exec.time = double(0L),
        extra = list())
      Map(function(id, type) {
        set(self$full.data, j = id, value = get(type, mode = "function")())
        },
        id = x.names,
        type = getParamTypes(par.set, df.cols = TRUE)
      )
      for (y.name in y.names) {
        set(self$full.data, j = y.name, value = numeric(0L))
      }
      names(minimize) = y.names
      self$x.names = x.names
      self$y.names = y.names
      self$par.set = par.set
      self$minimize = minimize
    },

    add = function(x, y, dob = NULL, eol = NA_integer_, msg = NA_character_, exec.time = NA_real_, extra = NULL) {
      if (!is.list(y)) {
        y = setNames(as.list(y), self$y.names)
      }
      assert_list(x, names = "strict")
      assert_list(y, names = "strict")
      self$full.data = rbindlist(
        list(self$full.data, c(list(dob = dob %??% (nrow(self$full.data) + 1), eol = eol, msg = msg, exec.time = exec.time, extra = list(extra)), x, y))
        )
    },
    x.names = NULL,
    y.names = NULL,
    par.set = NULL,
    minimize = NULL,
    full.data = NULL
  ),

  active = list(
    env = function() {
      self$data
    },
    data = function() {
      self$full.data
    }
  )
)

## overwrite creation

makeOptPathDF = function(par.set, y.names, minimize, add.transformed.x = FALSE, include.error.message = FALSE, include.exec.time = FALSE, include.extra = FALSE) {
  if (add.transformed.x == TRUE) {
    stop("add.transformed.x == TRUE not supported by OptPathNg")
  }
  if (include.error.message == FALSE) {
    stop("include.error.message == FALSE not supported by OptPathNg")
  }
  if (include.exec.time == FALSE) {
    stop("include.exec.time == FALSE not supported by OptPathNg")
  }
  if (include.extra == FALSE) {
    stop("include.extra == FALSE not supported by OptPathNg")
  }
  op = OptPathNg$new(par.set, y.names = y.names, minimize = minimize)
  return(op)
}

#' @export
addOptPathEl.OptPathNg = function(op, x, y, dob = getOptPathLength(op)+1L, eol = NA_integer_, error.message = NA_character_, exec.time = NA_real_, extra = NULL, check.feasible = FALSE) {
  if (isTRUE(check.feasible)) {
    warning("check.feasible is ignored for OptPathNg")
  }
  if (any(extractSubList(op$par.set$pars, "len") > 1)) {
    x = lapply(x, as.list)
    x = unlist(x, recursive = FALSE, use.names = FALSE)
    x = setNames(x, getParamIds(op$par.set, repeated = TRUE, with.nr = TRUE))
  }
  op$add(x = x, y = y, dob = dob, exec.time = exec.time, eol = eol, msg = error.message, extra = extra)
  invisible(op)
}
## overwrite getters of ParamHelpers::

#' @export
getOptPathLength.OptPathNg = function(op) {
  nrow(op$data)
}

#' @export
getOptPathExecTimes.OptPathNg = function(op, dob, eol) {
  if (!missing(dob) || !missing(eol)) {
    stop("dob and eol not supported for OptPathNg")
  }
  op$data$exec.time
}

#' @export
getOptPathX.OptPathNg = function(op, dob, eol) {
  if (!missing(dob) || !missing(eol)) {
    stop("dob and eol not supported for OptPathNg")
  }
  op$data[,op$x.names, with = FALSE]
}

#' @export
getOptPathY.OptPathNg = function(op, names, dob, eol, drop = TRUE) {
  if (!missing(dob) || !missing(eol)) {
    stop("dob, eol and drop not supported for OptPathNg")
  }
  names = names %??% op$y.names
  res = op$data[, names, with = FALSE]
  if (drop && ncol(res) == 1) {
    res[[1]]
  } else {
    as.matrix(res)
  }
}

#' @export
getOptPathDOB.OptPathNg = function(op, dob = NULL, eol = NULL) {
  dobeol.sub = getOptPathDobAndEolIndex(op, dob, eol)
  op$data$dob[dobeol.sub]
}

#' @export
getOptPathErrorMessages.OptPathNg = function(op, dob, eol) {
  if (!missing(dob) || !missing(eol)) {
    stop("dob and eol not supported for OptPathNg")
  }
  op$data$msg
}

#' @export
getOptPathEl.OptPathNg = function(op, index) {
  x = dfRowToList(df = getOptPathX(op), par.set = op$par.set, i = index)
  y = getOptPathY(op)
  if (is.matrix(y)) {
    y = y[index,]
  } else {
    y = y[index]
  }
  res = list(x = x, y = y, dob = op$data$dob[index], eol = op$data$eol[index], error.message = op$data$msg[index], exec.time = op$data$exec.time[index], extra = op$data$extra[[index]])
}

#not supported warnings

#' @export
getOptPathCol.OptPathNg = function(op, name, dob = op$env$dob, eol = op$env$eol) {
  stop("Not supported for OptPathNg!")
}

#' @export
getOptPathCols.OptPathNg = function(op, names, dob = op$env$dob, eol = op$env$eol, row.names = NULL) {
  stop("Not supported for OptPathNg!")
}

#' @export
getOptPathEOL.OptPathNg = function(op, dob = op$env$dob, eol = op$env$eol) {
  stop("Not supported for OptPathNg!")
}

# data.frame conversion

#' @export
as.data.frame.OptPathNg = function(x, row.names = NULL, optional, include.x = TRUE, include.y = TRUE, include.rest = TRUE, dob = NULL, eol = NULL, ...) {

  if (!missing(optional)) {
    stop("optional is not supported for OptPathNg")
  }

  dt = data.table::copy(x$data)

  dobeol.sub = getOptPathDobAndEolIndex(x, dob, eol)
  dt = dt[dobeol.sub, ]

    if (include.rest == FALSE) {
    dt[, c("dob", "eol", "msg", "exec.time", "extra"):=NULL]
  } else {
    extra = rbindlist(dt$extra, fill = TRUE)
    dt[, "extra" := NULL]
    dt = cbind(dt, extra)
  }
  if (include.x == FALSE) {
    dt[, x$x.names := NULL]
  }
  if (include.y == FALSE) {
    dt[, x$y.names := NULL]
  }


  as.data.frame(dt, ...)
}

# helpers
getOptPathDobAndEolIndex = function(op, dob = NULL, eol = NULL) {
  if (!is.null(dob)) {
    dob.sub = op$data$dob %in% dob
  } else {
    dob.sub = rep(TRUE, times = nrow(op$data))
  }

  if (!is.null(eol)) {
    eol.sub = op$data$eol %in% eol
  } else {
    eol.sub = rep(TRUE, times = nrow(op$data))
  }
  dob.sub & eol.sub
}



# WARNING: Obviously subsetting an OptPath can result in objects that do not resemble what we expect from an OptPath
`[.OptPathNg` = function(x, ...) {
  z = x$clone()
  z$data = '['(z$data, ...)
  z
}

`[[.OptPathNg` = function(x, ...) {
  z = x$clone()
  z$data = '[['(z$data, ...)
  z
}

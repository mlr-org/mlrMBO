## mlr-org/mlrng/attic/OptPath.R - 18.09.2017

#' @importFrom R6 R6Class
#' @import data.table
OptPathNg = R6Class(c("OptPathNg", "OptPath"),
  public = list(
    initialize = function(par.set, y.names = "y", minimize = TRUE) {
      x.names = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
      private$tab = data.table(
        dob = integer(0L),
        eol = integer(0L),
        msg = character(0L),
        exec.time = double(0L),
        extra = list())
      Map(function(id, type) {
        set(private$tab, j = id, value = get(type, mode = "function")())
        },
        id = x.names,
        type = getParamTypes(par.set, df.cols = TRUE)
      )
      for (y.name in y.names) {
        set(private$tab, j = y.name, value = numeric(0L))
      }
      names(minimize) = y.names
      self$x.names = x.names
      self$y.names = y.names
      self$par.set = par.set
      self$minimize = minimize
    },

    add = function(x, y, dob = NULL, eol = NA_integer_, msg = NA_character_, exec.time = NA_real_, extra = NULL) {
      if (private$cache.pos == length(private$cache))
        self$flush()

      cache.pos = private$cache.pos = private$cache.pos + 1L
      private$cache[[cache.pos]] = c(x, list(y = y, dob = dob %??% (nrow(private$tab) + cache.pos),
          eol = eol, msg = msg, exec.time = exec.time, extra = list(extra)))
    },

    flush = function() {
      if (private$cache.pos > 0L) {
        cached = rbindlist(head(private$cache, private$cache.pos), fill = TRUE)
        private$tab = rbindlist(list(private$tab, cached), fill = TRUE)
        setorderv(private$tab, "dob")
        private$cache.pos = 0L
      }
    },
    x.names = NULL,
    y.names = NULL,
    par.set = NULL,
    minimize = NULL
  ),

  active = list(
    data = function() {
      self$flush()
      private$tab
    },
    env = function() {
      self$data
    }
  ),

  private = list(
    cache.pos = 0L,
    cache = vector("list", 512L),
    tab = NULL
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

getOptPathLength.OptPathNg = function(op) {
  nrow(op$data)
}


getOptPathExecTimes.OptPathNg = function(op, dob, eol) {
  if (!missing(dob) || !missing(eol)) {
    error("dob and eol not supported for OptPathNg")
  }
  op$data$exec.time
}


getOptPathX.OptPathNg = function(op, dob, eol) {
  if (!missing(dob) || !missing(eol)) {
    error("dob and eol not supported for OptPathNg")
  }
  op$data[,op$x.names, with = FALSE]
}

getOptPathY.OptPathNg = function(op, names, dob, eol, drop) {
  if (!missing(dob) || !missing(eol) || !missing(drop)) {
    error("dob, eol and drop not supported for OptPathNg")
  }
  names = names %??% op$y.names
  res = op$data[, names, with = FALSE]
  if (ncol(res) == 1) {
    res[[1]]
  } else {
    as.matrix(res)
  }
}

getOptPathDob.OptPathNg = function(op, dob, eol) {
  if (!missing(dob) || !missing(eol)) {
    error("dob and eol not supported for OptPathNg")
  }
  op$data$dob
}


getOptPathErrorMessages.OptPathNg = function(op, dob, eol) {
  if (!missing(dob) || !missing(eol)) {
    error("dob and eol not supported for OptPathNg")
  }
  op$data$msg
}


getOptPathEl.OptPathNg = function(op, index) {
  x = dfRowToList(df = getOptPathX(op), par.set = op$par.set, i = index)
  y = getOptPathY(op)
  if (is.matrix(y)) {
    y = y[index,]
  } else {
    y = y[index]
  }
  res = list(x = x, y = y, dob = op$data$dob[index], eol = op$data$eol[index], error.message = op$data$msg[index], exec.time = op$data$exec.time[index], extra = op$data$extra[index])
}

#not supported warnings

getOptPathCol.OptPathNg = function(op, name, dob = op$env$dob, eol = op$env$eol) {
  error("Not supported for OptPathNg!")
}

getOptPathCols.OptPathNg = function(op, names, dob = op$env$dob, eol = op$env$eol, row.names = NULL) {
  error("Not supported for OptPathNg!")
}

getOptPathEOL.OptPathNg = function(op, dob = op$env$dob, eol = op$env$eol) {
  error("Not supported for OptPathNg!")
}

# data.frame conversion
as.data.frame.OptPathNg = function(x, row.names = NULL, optional, include.x = TRUE, include.y = TRUE, include.rest = TRUE, dob, eol, ...) {

  if (!missing(optional) || !missing(dob) || !missing(eol)) {
    stop("optional, dob or eol not supported for OptPathNg")
  }

  dt = data.table::copy(x$data)

    if (include.rest == FALSE) {
    dt[, c("dob", "eol", "msg", "exec.time", "extra"):=NULL]
  } else {
    extra = rbindlist(dt$extra)
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


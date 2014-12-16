
#' Plot method for multi-objective result objects.
#'
#' @param result [\code{MBOSingleObjResult}]\cr
#'   Multi-Objective mlrMBO result object.
#' @param infill.crit [\code{character(1)}]\cr
#'   Infill criterion used for optimization.
#' @param iters [\code{numeric} | NULL]\cr
#'   Vector of iteration which should be plotted one after another. If \code{NULL},
#'   which is the default, all iterations are plotted.
#' @param alpha [\code{logical(1)}]\cr
#'   Activates or deactivates the alpha fading for the parallel X-space plot. Default is \code{TRUE}.
#' @param pause [\code{logical(1)}]\cr
#'   Pause after each iteration?
#'   Default is \code{TRUE}.
#' @pause log.infill.crit [\code{logical(1)}]\cr
#'   Should the infill criterion be log-transformed? Default is \code{FALSE}.
#' @return Nothing
#FIXME: it would be more comfortable if the infill crit could be extracted from the result object. Maybe save control object in result?
plot.MBOMultiObjResult = function(result, infill.crit, iters = NULL, alpha = TRUE, pause = TRUE,
  log.infill.crit = FALSE) {
  requirePackages("GGally")
  requirePackages("ggplot2")
  requirePackages("gridExtra")
  
  assertClass(result, "MBOMultiObjResult")
  
  op = result$opt.path
  ref.point = apply(getOptPathY(op), 2, max) + 1
  if (is.null(iters))
    iters = unique(getOptPathDOB(op))
  assertIntegerish(iters)
  
  if (any(iters > max(getOptPathDOB(op))))
    stop("You want to plot an iteration that does not exist!")
  
  # FIXME: better way to get x-names?
  x.names = colnames(getOptPathX(result$opt.path))
  y.names = result$opt.path$y.names
  
  if (length(y.names) != 2)
    stop("Can only plot optimizations with 2 targets.")
  
  # initialize data.frames for hypervolume and crit value
  hv.data = makeDataFrame(ncol = 2, nrow = 0, col.types = "numeric", col.names = c("hv", "dob"))
  crit.data = makeDataFrame(ncol = 2, nrow = 0, col.types = "numeric", col.names = c("crit", "dob"))
  # Pre-Check: If log-scale of crit - all crit values positive or negative?
  extras = as.data.frame(op, include.x = FALSE, include.y = FALSE)
  if (infill.crit %nin% colnames(extras))
    stop("Specified infill.crit does not exist.")
  crits = extras[, infill.crit]
  if (log.infill.crit && any(crits < 0) && any(crits > 0))
    stop("You want to log the infill.crit, but it does contain both positive and negative values.")
  
  for (i in iters) {
    # get the data
    op.x = as.data.frame(op, include.y = FALSE, include.rest = FALSE, dob = 0:i)
    op.y = as.data.frame(op, include.x = FALSE, include.rest = FALSE, dob = 0:i)
    dob = getOptPathDOB(op, dob = 0:i)
    
    # plot 1: y-space
    op.y$type = as.factor(ifelse(dob == 0, "init", ifelse(dob == i, "prop", "seq")))
    pl1 = ggplot(op.y, aes(x = y_1, y = y_2, shape = type, colour = type))
    pl1 = pl1 + geom_point(size = 3)
    pl1 = pl1 + ggtitle("Y-Space")
    pl1 = pl1 + xlab(expression(y[1])) + ylab(expression(y[2]))
    pl1 = pl1 + theme(legend.position = "top")
    
    # plot 2: x-space
    args = list(columns = seq_along(x.names))
    if (alpha) {
      op.x$.alpha = if(i == 0)
        rep(1, length(dob)) else normalize(dob, "range", range = c(1 / (i + 1), 1))
      args$alphaLines = ".alpha"
    }
    op.x$type = op.y$type
    args$data = op.x
    args$groupColumn = ncol(op.x)
    args$mapping = aes(lwd = 1.5)
    pl2 = do.call(ggparcoord, args)
    pl2 = pl2 + ylab ("value divided by standard deviation")
    pl2 = pl2 + ggtitle("X-Space")
    pl2 = pl2 + guides(alpha = FALSE)
    pl2 = pl2 + theme(legend.position = "top", legend.margin = unit(0.05, "cm"))
    
    # plot 3 - dominated hypervolume
    hv = dominated_hypervolume(t(getOptPathParetoFront(result$opt.path, dob = 0:i)), ref = ref.point)
    hv.data = rbind(hv.data, data.frame(hv = hv, dob = i))
    pl3 = ggplot(hv.data, aes(x = dob, y = hv)) + 
      geom_point() + ggtitle(paste("Dominated Hypervolume, ref.point = (", 
        collapse((round(ref.point, 1)), sep = ", "), ")", sep = "")) + 
      theme(plot.title = element_text(size = rel(1)))
    if (nrow(hv.data) > 1)
      pl3 = pl3 + geom_line()
    

    # plot 4 - indicator value. ony available if iter != 0
    pl4 = NA
    if (i != 0) {
      crit = as.data.frame(op, include.x = FALSE, include.y = FALSE, dob = i)[, 5]
      # we want to log some crits. this must be user defined. since we have
      # some negated crits, replace them with positive values
      # we prechecked: eiter all or none crit value is negative
      if (log.infill.crit & any(crit < 0)) {
        crit = -crit
      }
      crit.data = rbind(crit.data, data.frame(crit = crit, dob = i))
      
      pl4 = ggplot(crit.data, aes(x = dob, y = crit))
      pl4 = pl4 + geom_point()
      pl4 = pl4 + ggtitle("Values of used infill criterion") 
      pl4 = pl4 + theme(plot.title = element_text(size = rel(1)))
      pl4 = pl4 + ylab(infill.crit)

      # plot lines only if more than one crit observation exists
      # if more than 1 value per dob exists (if prop.points > 1)
      # use mean value for lines
      if (length(unique(dob[dob != 0L])) > 1L) {
        d = data.frame(
          crit = tapply(crit.data$crit, crit.data$dob, mean),
          dob = unique(dob[dob != 0])
        )
        pl4 = pl4 + geom_line(data = d)
      }
      if (log.infill.crit) {
        pl4 = pl4 + scale_y_log10()
      }
      # Arrange 3 or 4 plot - depending if pl4 exists
      grid.arrange(pl1, pl2, pl3, pl4, nrow = 2, heights = c(2, 1))
    } else {
      grid.arrange(pl1, pl2, pl3, nrow = 2, heights = c(2, 1))
    }

    if(pause && i != max(iters))
      readline(prompt = "Press [enter] to continue")
  }
  return(invisible(NULL))
}
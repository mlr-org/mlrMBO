
# checks or sets lim.x and lim.y for mbo Plots. lim.x and lim.y are names lists,
# with names corresponding to the plots (XSPace, YSpace, CritPlot, HVPlot, 
# ExtraPlot1, ExtraPlot2), each list element must be NULL or numeric of length 2
# if NULL, the corresponding lims are calculated via the range of the corresponding
# values

# X limits for diagnostic plots will NOT be set. Since we have the number of
# iterations on the X axis here, the ggplot default always do what we want

getLimits <- function(lim.x, lim.y, result, iters, extra.measure, ref.point) {
  op = result$opt.path
  ctrl = result$control
  
  # Use the private ParamHelpers method to set lims for XSpace and YSpace
  tmp = ParamHelpers:::getOptPathLims(lim.x, lim.y, op, iters, 0.05)
  lim.x = tmp$lim.x
  lim.y = tmp$lim.y
  
  # Crit Plot
  # FIXME MSPOT is special
  
  # lim.x is iteration number here. if NULL, use ggplot defaults, else check
  if (is.null(lim.x$CritPlot)) {
    lim.x$CritPlot = c(NA_real_, NA_real_)
  } else {
    assertNumeric(lim.x$CritPlot, len = 2)
  }
  
  # if NULL, use range of observed crit, else check
  if (is.null(lim.y$CritPlot)) {
    vals = na.omit(getOptPathCol(op, ctrl$infill.crit, dob = 0:max(iters)))
    if (length(vals) == 0L) {
      lim.y$CritPlot = c(NA, NA)  
    } else {
      crit.lims = range(vals, finite = TRUE)
      crit.lims = c(-1, 1) * 0.05 * diff(crit.lims) + crit.lims
      lim.y$CritPlot = crit.lims
    }
  } else {
    assertNumeric(lim.y$CritPlot, len = 2)
  } 
  
  # HV Plot
  # iteration
  if (ctrl$number.of.target > 1L) {
    # lim.x is iteration number here. if NULL, use ggplot defaults, else check
    if (is.null(lim.x$HVPlot)) {
      lim.x$HVPlot = c(NA_real_, NA_real_)
    } else {
      assertNumeric(lim.x$HVPlot, len = 2)
    }
    
    # if null, use min and max of hypervolume, else check
    if (is.null(lim.y$HVPlot)) {
      # HV is monotonically increasing - just get the value for first and last
      hv.min = getDominatedHV(getOptPathParetoFront(op = op, dob = 0), ref.point, ctrl$minimize)
      hv.max = getDominatedHV(getOptPathParetoFront(op = op, dob = 0:max(iters)), ref.point, ctrl$minimize)
      lim.y$HVPlot = c(hv.min, hv.max)
    } else {
      assertNumeric(lim.y$HVPlot, len = 2)
    }
  }
  
  # Extra Measure Plots
  if (length(extra.measure) > 0L) {
    # lim.x is iteration number here. if NULL, use ggplot defaults, else check
    if (is.null(lim.x$ExtraPlot1)) {
      # If NULL, we do nothing and use the ggplot defaults later
      lim.x$ExtraPlot1 = c(NA_real_, NA_real_)
    } else {
      assertNumeric(lim.x$ExtraPlot1, len = 2)
    }
    
    # if NULL, use range of observed measure, else check
    if (is.null(lim.y$ExtraPlot1)) {
      vals = na.omit(getOptPathCol(op, extra.measure[1], dob = 0:max(iters)))
      if (length(vals) == 0L) {
        lim.y$ExtraPlot1 = c(NA_real_, NA_real_)
      } else {
        extra1.lims = range(vals, finite = TRUE)
        lim.y$ExtraPlot1 = c(-1, 1) * 0.05 * diff(extra1.lims) + extra1.lims
      }
    } else {
      assertNumeric(lim.y$ExtraPlot1, len = 2)
    }
  }
  
  if (length(extra.measure) > 1L) {
    # If NULL, we do nothing and use the ggplot defaults later
    if (is.null(lim.x$ExtraPlot2)) {
      lim.x$ExtraPlot2 = c(NA_real_, NA_real_)
    } else {
      assertNumeric(lim.x$ExtraPlot2, len = 2)
    }
    
    # if NULL, use range of observed measure, else check
    if (is.null(lim.y$ExtraPlot2)) {
      vals = na.omit(getOptPathCol(op, extra.measure[2], dob = 0:max(iters)))
      if (length(vals) == 0L) {
        lim.y$ExtraPlot2 = c(NA_real_, NA_real_)  
      } else {
        extra2.lims = range(vals, finite = TRUE)
        lim.y$ExtraPlot2 = c(-1, 1) * 0.05 * diff(extra2.lims) + extra2.lims
      }
    } else {
      assertNumeric(lim.y$ExtraPlot2, len = 2)
    }
    
    
  }
  return(list(lim.x = lim.x, lim.y = lim.y))
}


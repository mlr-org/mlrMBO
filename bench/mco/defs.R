# GENERAL
FEVALS = function(dimx) 40 * dimx
REPLS = 20L
INIT_DESIGN_POINTS = function(dimx) {
  return(4 * dimx)
}
PARALLEL_PROP_POINTS = c(1L, 4L)
MBO_ITERS = function(dimx, prop.points)
  (FEVALS(dimx) - INIT_DESIGN_POINTS(dimx)) / prop.points

# FOCUSSEARCH
FOCUSSEARCH_POINTS = 1000L
FOCUSSEARCH_MAXIT = 3L
FOCUSSEARCH_RESTARTS =  3L

# general algo stuff
LCB_PI = 0.5    # mspot_lcb + dib (all)
MULTICRIT_REFPOINT = "all" # mspot (all) + dib.sms
MULTICRIT_REFPOINT_OFFSET = 1 # mspot (all) + dib.sms

# nsga2 baseline comparison
BASELINE_NSGA2_POPSIZE = function(dimx) dimx * 4 # must be a multiple of 4, set like init design
# the next lines assume that mco version >= 1.0??? is installed
BASELINE_NSGA2_GENERATIONS1 = function(dimx) FEVALS(dimx) / BASELINE_NSGA2_POPSIZE(dimx) - 1L
BASELINE_NSGA2_GENERATIONS2 = function(dimx) 10 * FEVALS(dimx) / BASELINE_NSGA2_POPSIZE(dimx) - 1L

# randomSearch baseline comparison
BASELINE_RANDOMSEARCH_BUDGET1 = function(dimx) FEVALS(dimx) - INIT_DESIGN_POINTS(dimx)
BASELINE_RANDOMSEARCH_BUDGET2 = function(dimx) 10 * FEVALS(dimx) - - INIT_DESIGN_POINTS(dimx)

# parego
PAREGO_RHO = 0.05
PAREGO_SAMPLE_MORE_WEIGHTS = 5L
PAREGO_CRIT = "ei"

# mspot
MSPOT_NSGA2_GENERATIONS = 90L
MSPOT_NSGA2_POPSIZE = 100L

# dib:
DIB_SMS_EPS = NULL # adaptive




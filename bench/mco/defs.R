FEVALS = 200L

REPLS = 10L

# nsga2
POPSIZE = 20L # must be a multiple of 4
GENERATIONS = as.integer(FEVALS/POPSIZE)

# parego
INIT_DESIGN_POINTS = 20L
PROP_POINTS = 20L
ITERS = as.integer((FEVALS - INIT_DESIGN_POINTS)/PROP_POINTS)


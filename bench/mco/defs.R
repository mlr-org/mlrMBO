FEVALS = 200L

REPLS = 10L

# nsga2
POPSIZE = 20L # must be a multiple of 4
stopifnot(POPSIZE %% 4 == 0)
stopifnot(FEVALS %% POPSIZE == 0)
GENERATIONS1 = FEVALS / POPSIZE
GENERATIONS2 = GENERATIONS1 * 10L

# parego
INIT_DESIGN_POINTS = 40L
PROP_POINTS = 5L
stopifnot((FEVALS - INIT_DESIGN_POINTS) %% PROP_POINTS == 0)
ITERS = (FEVALS - INIT_DESIGN_POINTS) / PROP_POINTS

# r2 indicator
dim1 = seq(0, 1, 0.01)
dim2 = 1 - dim1
WEIGHTS = rbind(dim1, dim2)

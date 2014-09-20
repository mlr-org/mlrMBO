# GENERAL
FEVALS = 40L # 40 * d
REPLS = 10L # 10
INIT_DESIGN_POINTS = 4L # 4 * d
PARALLEL_PROP_POINTS = c(1L, 5L) # 1 und 5
stopifnot((FEVALS - INIT_DESIGN_POINTS) %% PARALLEL_PROP_POINTS == 0)

# FOCUSSEARCH
FOCUSSEARCH_POINTS = 1000L # 1000
FOCUSSEARCH_MAXIT = 3L # 3
FOCUSSEARCH_RESTARTS = 3L # 3

# Crit
LCB_PI = 0.5

# nsga2 baseline comparison
BASELINE_NSGA2_POPSIZE = 4 # must be a multiple of 4, # 4*d, pareto front von allem
stopifnot(BASELINE_NSGA2_POPSIZE %% 4 == 0)
stopifnot(FEVALS %% BASELINE_NSGA2_POPSIZE == 0)
# subtract 2 for initial FEVALS of NSGA2?
BASELINE_NSGA2_GENERATIONS1 = FEVALS / BASELINE_NSGA2_POPSIZE - 2
BASELINE_NSGA2_GENERATIONS2 = BASELINE_NSGA2_GENERATIONS1 * 10L - 2

# parego
stopifnot((FEVALS - INIT_DESIGN_POINTS) %% PARALLEL_PROP_POINTS == 0)
# ITERS = (FEVALS - INIT_DESIGN_POINTS) / PROP_POINTS

# mspot
MSPOT_NSGA2_GENERATIONS = 90L
MSPOT_NSGA2_POPSIZE = 100L


# parego 1p und mp: s-wert: viele, rho = 0.05, sample_more_weights, crit = "ei"

# dib: sms: 1p: eps (adaptiv), lcb-lambda (tobias formel), refp (adaptiv), PI = 0.5
# dib: sms: mp: eps (adaptiv), lcb-lambda (tobias formel), refp (adaptiv), PI = 0.5

# dib: eps: 1p: lcb-lambda (tobias formel), PI = 0.5
# dib: eps: mp: lcb-lambda (tobias formel), PI = 0.5

# mspot
# nsga2 params
# mean, lcb, ei
# lcb mit PI = 0.5
# ref adaptiv



##########################
# Suzanne Dufault
# Running Count-Based Models
# With HCSB
##########################
library(tidyr)
library(dplyr)
library(splitstackshape)
library(tibble)
library(geepack)
library(lme4)
library(here)
library(purrr)
library(furrr)

load(here("output", "2019-06-25_simulation-output-1000.RData"))

source(here("multinom_functions", "performance-evaluation-function.R"))
# source(here("multinom_functions", "2019-03-08_agg-OR-function.R"))
# source(here("multinom_functions", "2019-03-08_me-gee-function.R"))
source(here("multinom_functions", "2019-11-19_me-gee-function-counts.R"))
# source(here("multinom_functions", "2019-03-08_test-positive-function.R"))

plan(multiprocess)

# Running the models on each simulated dataset
multinom_hcsb_1000_count <- performance_function(sims, 
                                           m = 12, 
                                           zstar = 1.96, 
                                           estimator = "ME.GEE.COUNT")

save(multinom_hcsb_1000_count, file = "output/2019-06-25_multinom_hcsb_1000_count.RData")
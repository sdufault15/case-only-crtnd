##########################
# Suzanne Dufault
# Setting up Multinomial Sampling
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

load(here("data", "Random10000Allocations.RData"))
data <- Random10000Allocations %>% select(-X)
# test.data <- data %>%
#   select(clust:tx10) %>%
#   filter(Period %in% c("03_05", "05_06"))

periods <- sort(unique(data$Period))
# periods.test <- droplevels(periods[1:2])

source(here("multinom_functions", "generate_sample_function.R"))
source(here("multinom_functions", "multinomial_sample_function_hcsb.R"))
source(here("multinom_functions", "performance-evaluation-function.R"))
source(here("multinom_functions", "sample_size_function.R"))

source(here("multinom_functions", "2019-03-08_agg-OR-function.R"))
source(here("multinom_functions", "2019-03-08_me-gee-function.R"))
source(here("multinom_functions", "2019-11-19_me-gee-function-counts.R"))
source(here("multinom_functions", "2019-03-08_test-positive-function.R"))

# Simulating the Datasets
plan(multiprocess)
sims <- future_map_dfr(c(0.8, 0.2),
    ~sample_size_function(df = data, 
                          periods = periods.test, 
                          lambdas = .x, 
                          hcsb = c(1, 0.95, 0.85, 0.5), 
                          ns = 1000, 
                          print = FALSE),
    .progress = TRUE)
save(sims, file = "output/2019-11-21_simulation-output-1000_rr-08-02.RData")


# Running the models on each simulated dataset
multinom_hcsb_1000 <- performance_function(sims, 
                                           m = 12, 
                                           zstar = 1.96, 
                                           estimator = c("AggOR", 
                                                         "ME.GEE", 
                                                         "TP",
                                                         "ME.GEE.COUNT"))
save(multinom_hcsb_1000, file = "output/2019-06-25_multinom_hcsb_1000_rr-08-02.RData")


##########################
# Suzanne Dufault
# Setting up Multinomial Sampling
##########################
library(tidyr)
library(dplyr)
library(splitstackshape)
library(tibble)
library(geepack)
library(lme4)
library(here)

load(here("data", "Random10000Allocations.RData"))
data <- Random10000Allocations %>% select(-X)
# test.data <- data %>% 
#   select(clust:tx20) %>%
#   filter(Period %in% c("03_05", "05_06"))


periods <- sort(unique(data$Period))

source(here("multinom_functions", "generate_sample_function.R"))
source(here("multinom_functions", "multinomial_sample_function.R"))
source(here("multinom_functions", "performance-evaluation-function.R"))
source(here("multinom_functions", "sample_size_function.R"))

source(here("multinom_functions", "2019-03-08_agg-OR-function.R"))
source(here("multinom_functions", "2019-03-08_me-gee-function.R"))
source(here("multinom_functions", "2019-03-08_test-positive-function.R"))


sims <- sample_size_function(df = data, periods = periods, lambdas = c(1, 0.6, 0.5, 0.4, 0.3), ns = 1000, print = FALSE)
multinom_no_hcsb_1000 <- performance_function(sims, m = 12, zstar = 1.96, estimator = c("AggOR", "ME.GEE", "TP"))
save(multinom_no_hcsb_1000, file = "output/2019-03-18_multinom_no_hcsb_1000.RData")
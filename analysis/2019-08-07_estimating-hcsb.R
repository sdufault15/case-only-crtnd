# Estimation of HCSB 

library(tidyr)
library(dplyr)
library(furrr)
library(purrr)
library(here)

# Data
load(here("data", "case-only-multinom/2019-06-25_multinom_hcsb_1000.RData"))
source(here("lib","multinom_functions/2019-03-08_test-positive-function.R"))

# Extract the modified data
data_lists <- multinom_hcsb_1000$data[c(which(multinom_hcsb_1000$lambda == 1))] 

# will take at least 24 minutes to run
estimates <- lapply(seq_along(data_lists), function(x){
  temp <- data_lists[[x]]
  temp <- temp %>%
    mutate(cases = OFIs) # replacing the case counts with the OFI counts so that TP will estimate alpha_{hcsb} rather than lambda_{tp}
  test_positive_function(df = temp, m = 12, zstar = 1.96)}
  )

# Take output and put it into data frame
output <- data.frame(matrix(unlist(estimates), ncol = 3, byrow = TRUE))
names(output) <- c("estimate", "sd", "pval")

output <- data.frame(lambda = multinom_hcsb_1000$lambda[c(which(multinom_hcsb_1000$lambda == 1))],
                     hcsb = multinom_hcsb_1000$hcsb[c(which(multinom_hcsb_1000$lambda == 1))], 
                     output)

save(output, 
     file = here("data", "case-only-multinom/2019-08-07_hcsb-estimation.RData"))


# Bias

output %>%
  rowwise() %>%
  mutate(bias = estimate - hcsb) %>%
  group_by(hcsb) %>%
  summarize(bias = mean(bias))


# Coverage
output %>%
  rowwise() %>%
  mutate(coverage = between(log(hcsb), 
                            log(estimate) - 1.96*sd,
                            log(estimate) + 1.96*sd)) %>%
  group_by(hcsb) %>%
  summarize(coverage = mean(coverage))

# Coverage
output %>%
  group_by(hcsb) %>%
  summarize(power = mean(pval <= 0.05))

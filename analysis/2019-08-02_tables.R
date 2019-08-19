library(tidyverse)
library(here)
# library(ggpubr)
# library(latex2exp)
library(xtable)
# myColors <- c("#d1b086", "#58ACB9", "#C0C3C3", "#50603F", "#969892")

# Don't waste time reloading data
ifelse(!exists("multinom_hcsb_1000"), 
               load(here("data","case-only-multinom/2019-06-25_multinom_hcsb_1000.RData")),
                    "Data already loaded in environment.")

############
# Bias
############
bias_prep <- multinom_hcsb_1000 %>%
  filter(hcsb == 1) %>%
  select(lambda, OR.agg, OR.gee, OR.me, OR.tp) %>%
  gather("method", "value", OR.agg:OR.tp) %>%
  group_by(lambda, method) %>%
  summarize(estimate = mean(value)) %>%
  mutate(bias = estimate - lambda) 

bias_tab <- bias_prep %>% 
  select(lambda, method, bias) %>%
  spread(method, bias) 

bias_tab$lambda <- paste0("RR = ", c(seq(0.3,0.6,by = 0.1), 1))
names(bias_tab) <- c(NA, "Aggregate OR", "GEE", "Mixed Effects", "Test-Positive Only")
xtable(bias_tab,
       digits = 4,
       caption = "The bias for 10,000 permuted intervention allocations across 9 time periods of historical data with samples of 1,000 cases and 4,000 controls.") %>%
  print(include.rownames = FALSE)


############
# Power
############

power_prep <- multinom_hcsb_1000 %>%
  filter(hcsb == 1) %>%
  group_by(lambda) %>%
  summarize(sig.agg = mean(sig.agg),
            sig.gee = mean(pval.gee <= 0.05),
            sig.me = mean(pval.me <= 0.05),
            sig.tp = mean(pval.tp <= 0.05))  

power_prep$lambda <- paste0("RR = ", c(seq(0.3,0.6,by = 0.1), 1))
names(power_prep) <- c(NA, "Aggregate OR", "GEE", "Mixed Effects", "Test-Positive Only")
xtable(power_prep,
       digits = 4,
       caption = "The power for 10,000 permuted intervention allocations across 9 time periods of historical data with samples of 1,000 cases and 4,000 controls.") %>%
  print(include.rownames = FALSE)

############
# Çoverage
############

coverage_fun <- function(truth, estimate, stdev){
  return(between(truth, 
          estimate - 1.96*stdev,
          estimate + 1.96*stdev))
}

coverage_prep <- multinom_hcsb_1000 %>%
  filter(hcsb == 1) %>%
  mutate(row_id = row_number()) %>%
  group_by(row_id) %>%
  # Standard deviations are already on the log scale
  mutate(coverage.agg = coverage_fun(log(lambda), log(OR.agg), sd.agg),
         coverage.gee = coverage_fun(log(lambda), log(OR.gee), sd.gee),
         coverage.me =  coverage_fun(log(lambda), log(OR.me), sd.me),
         coverage.tp =  coverage_fun(log(lambda), log(OR.tp), sd.tp)) %>%
  select(lambda, coverage.agg, coverage.gee, coverage.me, coverage.tp)

coverage_tab <- coverage_prep %>%
  ungroup() %>%
  group_by(lambda) %>%
  summarize(cov.agg = mean(coverage.agg, na.rm = TRUE),
            cov.gee = mean(coverage.gee, na.rm = TRUE),
            cov.me =  mean(coverage.me , na.rm = TRUE),
            cov.tp =  mean(coverage.tp , na.rm = TRUE))

coverage_tab$lambda <- paste0("RR = ", c(seq(0.3,0.6,by = 0.1), 1))
names(coverage_tab) <- c(NA, "Aggregate OR", "GEE", "Mixed Effects", "Test-Positive Only")
xtable(coverage_tab,
       digits = 4,
       caption = "The coverage for 10,000 permuted intervention allocations across 9 time periods of historical data with samples of 1,000 cases and 4,000 controls.") %>%
  print(include.rownames = FALSE)


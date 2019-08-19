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


# LARGE TABLE
bias <- multinom_hcsb_1000 %>%
  select(lambda, hcsb, OR.agg, OR.gee, OR.me, OR.tp) %>%
  gather("method", "value", OR.agg:OR.tp) %>%
  group_by(lambda, hcsb, method) %>%
  summarize(estimate = mean(value)) %>%
  mutate(bias = estimate - lambda)

agg <- bias %>%
  select(method, hcsb, bias) %>%
  spread(method, bias)

l1 <- agg %>%
  filter(lambda == 1)
l06 <- agg %>%
  filter(lambda == 0.6)
l05 <- agg %>%
  filter(lambda == 0.5)
l04 <- agg %>%
  filter(lambda == 0.4)
l03 <- agg %>%
  filter(lambda == 0.3)

bias_all <- rbind(l1[4:1,],
      l06[4:1,],
      l05[4:1,],
      l04[4:1,],
      l03[4:1,]) 
bias_all %>%
  xtable(digits=c(0,1,2,rep(4,4)),
         caption = "The bias in estimating the intervention relative risk for each method over the 10,000 intervention allocations applied to the 9 historical time periods with 1,000 cases and 4,000 controls as differential health-care--seeking behavior increases in severity. The largest biases are denoted in red.") %>%
  print(include.rownames = FALSE)

# PLOT
bias %>%
  mutate(HCSB = as.factor(hcsb),
         LAMBDA = as.factor(lambda)) %>%
  mutate(method = case_when(method == "OR.agg" ~ "Aggregate OR",
                            method == "OR.gee" ~ "GEE",
                            method == "OR.me" ~ "Mixed Effects",
                            method == "OR.tp" ~ "Test Positive Only")) %>%

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

# LARGE TABLE

power <- multinom_hcsb_1000 %>%
  select(lambda, hcsb, pval.gee, pval.me, pval.tp) %>%
  gather("method", "value", pval.gee:pval.tp) %>%
  group_by(lambda, hcsb, method) %>%
  summarize(power = mean(value <= 0.05))

power_agg <- multinom_hcsb_1000 %>%
  select(lambda, hcsb, sig.agg) %>%
  group_by(lambda, hcsb) %>%
  summarize(power = mean(sig.agg, na.rm = TRUE)) %>%
  mutate(method = "pval.agg")

power <- bind_rows(power, power_agg)

agg <- power %>%
  select(method, hcsb, power) %>%
  spread(method, power)

l1 <- agg %>%
  filter(lambda == 1)
l06 <- agg %>%
  filter(lambda == 0.6)
l05 <- agg %>%
  filter(lambda == 0.5)
l04 <- agg %>%
  filter(lambda == 0.4)
l03 <- agg %>%
  filter(lambda == 0.3)

power_all <- rbind(l1[4:1,],
                  l06[4:1,],
                  l05[4:1,],
                  l04[4:1,],
                  l03[4:1,]) 
power_all %>%
  xtable(digits=c(0,1,2,rep(4,4)),
         caption = "The power in estimating the intervention relative risk for each method over the 10,000 intervention allocations applied to the 9 historical time periods with 1,000 cases and 4,000 controls as differential health-care--seeking behavior increases in severity. The largest biases are denoted in red.") %>%
  print(include.rownames = FALSE)


# PLOT
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

# LARGE TABLE

coverage <- multinom_hcsb_1000 %>%
  rowwise() %>%
  mutate(coverage.agg = coverage_fun(log(lambda), log(OR.agg), sd.agg),
         coverage.gee = coverage_fun(log(lambda), log(OR.gee), sd.gee),
         coverage.me =  coverage_fun(log(lambda), log(OR.me), sd.me),
         coverage.tp =  coverage_fun(log(lambda), log(OR.tp), sd.tp)) %>%
  select(lambda, hcsb, coverage.agg, coverage.gee, coverage.me, coverage.tp) %>%
  gather("method", "value", coverage.agg:coverage.tp) %>%
  group_by(lambda, hcsb, method) %>%
  summarize(coverage = mean(value, na.rm = TRUE))


agg <- coverage %>%
  ungroup() %>%
  spread(method, coverage)

l1 <- agg %>%
  filter(lambda == 1)
l06 <- agg %>%
  filter(lambda == 0.6)
l05 <- agg %>%
  filter(lambda == 0.5)
l04 <- agg %>%
  filter(lambda == 0.4)
l03 <- agg %>%
  filter(lambda == 0.3)

coverage_all <- rbind(l1[4:1,],
                   l06[4:1,],
                   l05[4:1,],
                   l04[4:1,],
                   l03[4:1,]) 
coverage_all %>%
  xtable(digits=c(0,1,2,rep(4,4)),
         caption = "The coverage in estimating the intervention relative risk for each method over the 10,000 intervention allocations applied to the 9 historical time periods with 1,000 cases and 4,000 controls as differential health-care--seeking behavior increases in severity. The largest biases are denoted in red.") %>%
  print(include.rownames = FALSE)

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


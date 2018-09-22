###############################
# Suzanne Dufault
# All HCSB runs including OR estimation
# September 22, 2018
###############################

load("Random10000Allocations.RData")
dta <- subset(Random10000Allocations, select = -X)
source("lib/txtSetFunction.R")
source("lib/case-only-function.R")
library(dplyr)

period1 <- c("03_05", "05_06", "06_07", "07_08", "08_10", "10_11", "11_12", "12_13", "13_14")

## No Differential HCSB
rr1 <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 1)
save(rr1, file = "case-only/all-rr1-hcsb1-09222018.RData")
rr6 <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 1)
save(rr6, file = "case-only/all-rr6-hcsb1-09222018.RData")
rr5 <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 1)
save(rr5, file = "case-only/all-rr5-hcsb1-09222018.RData")
rr4 <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 1)
save(rr4, file = "case-only/all-rr4-hcsb1-09222018.RData")
rr3 <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 1)
save(rr3, file = "case-only/all-rr3-hcsb1-09222018.RData")

# Low HCSB (5%)
rr1l <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.95)
save(rr1l, file = "case-only/all-rr1-hcsb-LOW-09222018.RData")
rr6l <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.95)
save(rr6l, file = "case-only/all-rr6-hcsb-LOW-09222018.RData")
rr5l <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.95)
save(rr5l, file = "case-only/all-rr5-hcsb-LOW-09222018.RData")
rr4l <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.95)
save(rr4l, file = "case-only/all-rr4-hcsb-LOW-09222018.RData")
rr3l <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.95)
save(rr3l, file = "case-only/all-rr3-hcsb-LOW-09222018.RData")

# Medium HCSB (15%)
rr1m <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.85)
save(rr1m, file = "case-only/all-rr1-hcsb-MED-09222018.RData")
rr6m <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.85)
save(rr6m, file = "case-only/all-rr6-hcsb-MED-09222018.RData")
rr5m <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.85)
save(rr5m, file = "case-only/all-rr5-hcsb-MED-09222018.RData")
rr4m <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.85)
save(rr4m, file = "case-only/all-rr4-hcsb-MED-09222018.RData")
rr3m <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.85)
save(rr3m, file = "case-only/all-rr3-hcsb-MED-09222018.RData")

# High HCSB (50%)
rr1h <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.5)
save(rr1h, file = "case-only/all-rr1-hcsb-HIGH-09222018.RData")
rr6h <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.5)
save(rr6h, file = "case-only/all-rr6-hcsb-HIGH-09222018.RData")
rr5h <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.5)
save(rr5h, file = "case-only/all-rr5-hcsb-HIGH-09222018.RData")
rr4h <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.5)
save(rr4h, file = "case-only/all-rr4-hcsb-HIGH-09222018.RData")
rr3h <- case_only_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.5)
save(rr3h, file = "case-only/all-rr3-hcsb-HIGH-09222018.RData")

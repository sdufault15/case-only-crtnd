###############################
# Suzanne Dufault
# All HCSB runs for Test-Postive Fraction and Random Effects
# October 5, 2018
###############################

load("Random10000Allocations.RData")
dta <- subset(Random10000Allocations, select = -X)
library(doParallel)
source("lib/txtSetFunction.R")
source("lib/quadraticFunction.R")
source("lib/hcsb-tpf-re-function-2.R")

period1 <- c("03_05", "05_06", "06_07", "07_08", "08_10", "10_11", "11_12", "12_13", "13_14")

cl <- makeCluster(16)
registerDoParallel(cl)

## No Differential HCSB
rr1c <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 1)
save(rr1c, file = "case-only-comparison/all-comp-rr1-hcsb1-1052018.RData")
rr6c <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 1)
save(rr6c, file = "case-only-comparison/all-comp-rr6-hcsb1-1052018.RData")
rr5c <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 1)
save(rr5c, file = "case-only-comparison/all-comp-rr5-hcsb1-1052018.RData")
rr4c <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 1)
save(rr4c, file = "case-only-comparison/all-comp-rr4-hcsb1-1052018.RData")
rr3c <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 1)
save(rr3c, file = "case-only-comparison/all-comp-rr3-hcsb1-1052018.RData")

# Low HCSB (5%)
rr1lc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.95)
save(rr1lc, file = "case-only-comparison/all-comp-rr1-hcsb-LOW-1052018.RData")
rr6lc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.95)
save(rr6lc, file = "case-only-comparison/all-comp-rr6-hcsb-LOW-1052018.RData")
rr5lc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.95)
save(rr5lc, file = "case-only-comparison/all-comp-rr5-hcsb-LOW-1052018.RData")
rr4lc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.95)
save(rr4lc, file = "case-only-comparison/all-comp-rr4-hcsb-LOW-1052018.RData")
rr3lc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.95)
save(rr3lc, file = "case-only-comparison/all-comp-rr3-hcsb-LOW-1052018.RData")

# Medium HCSB (15%)
rr1mc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.85)
save(rr1mc, file = "case-only-comparison/all-comp-rr1-hcsb-MED-1052018.RData")
rr6mc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.85)
save(rr6mc, file = "case-only-comparison/all-comp-rr6-hcsb-MED-1052018.RData")
rr5mc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.85)
save(rr5mc, file = "case-only-comparison/all-comp-rr5-hcsb-MED-1052018.RData")
rr4mc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.85)
save(rr4mc, file = "case-only-comparison/all-comp-rr4-hcsb-MED-1052018.RData")
rr3mc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.85)
save(rr3mc, file = "case-only-comparison/all-comp-rr3-hcsb-MED-1052018.RData")

# High HCSB (50%)
rr1hc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.5)
save(rr1hc, file = "case-only-comparison/all-comp-rr1-hcsb-HIGH-1052018.RData")
rr6hc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.5)
save(rr6hc, file = "case-only-comparison/all-comp-rr6-hcsb-HIGH-1052018.RData")
rr5hc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.5)
save(rr5hc, file = "case-only-comparison/all-comp-rr5-hcsb-HIGH-1052018.RData")
rr4hc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.5)
save(rr4hc, file = "case-only-comparison/all-comp-rr4-hcsb-HIGH-1052018.RData")
rr3hc <- hcsb_tpf_re_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.5)
save(rr3hc, file = "case-only-comparison/all-comp-rr3-hcsb-HIGH-1052018.RData")

stopCluster()

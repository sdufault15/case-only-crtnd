###############################
# Suzanne Dufault
# All HCSB runs for Test-Postive Fraction and Random Effects
# October 9, 2018
# Sequential
###############################

load("Random10000Allocations.RData")
dta <- subset(Random10000Allocations, select = -X)
source("lib/txtSetFunction.R")
source("lib/quadraticFunction.R")
source("lib/hcsb-tpf-function-par.R")

period1 <- c("03_05", "05_06", "06_07", "07_08", "08_10", "10_11", "11_12", "12_13", "13_14")

# MED HCSB (85%)
rr1m.tpf <- hcsb_tpf_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.85)
save(rr1m.tpf, file = "case-only-comparison/tpf-comp-rr1-hcsb-MED-1082018.RData")
rr6m.tpf <- hcsb_tpf_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.85)
save(rr6m.tpf, file = "case-only-comparison/tpf-comp-rr6-hcsb-MED-1082018.RData")
rr5m.tpf <- hcsb_tpf_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.85)
save(rr5m.tpf, file = "case-only-comparison/tpf-comp-rr5-hcsb-MED-1082018.RData")
rr4m.tpf <- hcsb_tpf_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.85)
save(rr4m.tpf, file = "case-only-comparison/tpf-comp-rr4-hcsb-MED-1082018.RData")
rr3m.tpf <- hcsb_tpf_function(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.85)
save(rr3m.tpf, file = "case-only-comparison/tpf-comp-rr3-hcsb-MED-1082018.RData")
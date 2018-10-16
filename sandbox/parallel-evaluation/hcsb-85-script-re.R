###############################
# Suzanne Dufault
# All HCSB runs for Random Effects
# October 11, 2018
# Sequential
###############################

load("Random10000Allocations.RData")
dta <- subset(Random10000Allocations, select = -X)
source("lib/txtSetFunction.R")
source("lib/hcsb-re-function-par.R")

period1 <- c("03_05", "05_06", "06_07", "07_08", "08_10", "10_11", "11_12", "12_13", "13_14")

# MED HCSB (85%)
rr1m.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.85)
save(rr1m.re, file = "case-only-comparison/re-comp-rr1-hcsb-MED-10112018.RData")
rr6m.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.85)
save(rr6m.re, file = "case-only-comparison/re-comp-rr6-hcsb-MED-10112018.RData")
rr5m.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.85)
save(rr5m.re, file = "case-only-comparison/re-comp-rr5-hcsb-MED-10112018.RData")
rr4m.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.85)
save(rr4m.re, file = "case-only-comparison/re-comp-rr4-hcsb-MED-10112018.RData")
rr3m.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.85)
save(rr3m.re, file = "case-only-comparison/re-comp-rr3-hcsb-MED-10112018.RData")

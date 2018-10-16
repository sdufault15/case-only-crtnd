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

# HIGH HCSB (50%)
rr1h.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.5)
save(rr1h.re, file = "case-only-comparison/re-comp-rr1-hcsb-HIGH-10112018.RData")
rr6h.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.5)
save(rr6h.re, file = "case-only-comparison/re-comp-rr6-hcsb-HIGH-10112018.RData")
rr5h.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.5)
save(rr5h.re, file = "case-only-comparison/re-comp-rr5-hcsb-HIGH-10112018.RData")
rr4h.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.5)
save(rr4h.re, file = "case-only-comparison/re-comp-rr4-hcsb-HIGH-10112018.RData")
rr3h.re <- hcsb_re_function_par(data = dta, period = period1, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.5)
save(rr3h.re, file = "case-only-comparison/re-comp-rr3-hcsb-HIGH-10112018.RData")

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
source("lib/hcsb-tpf-re-function-par.R")

period1 <- c("03_05", "05_06", "06_07", "07_08", "08_10", "10_11", "11_12", "12_13", "13_14")

cl <- makeCluster(12)
registerDoParallel(cl)

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
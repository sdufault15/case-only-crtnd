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

cl <- makeCluster(12)
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
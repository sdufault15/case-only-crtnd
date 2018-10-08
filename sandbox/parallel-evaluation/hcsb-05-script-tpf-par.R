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

library(doParallel)
cl <- makeCluster(8)
registerDoParallel(cl)

# High HCSB (50%)
rr1h.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.5)}
save(rr1h.tpf, file = "case-only-comparison/par-tpf-comp-rr1-hcsb-HIGH-1082018.RData")
rr6h.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.5)}
save(rr6h.tpf, file = "case-only-comparison/par-tpf-comp-rr6-hcsb-HIGH-1082018.RData")
rr5h.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.5)}
save(rr5h.tpf, file = "case-only-comparison/par-tpf-comp-rr5-hcsb-HIGH-1082018.RData")
rr4h.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.5)}
save(rr4h.tpf, file = "case-only-comparison/par-tpf-comp-rr4-hcsb-HIGH-1082018.RData")
rr3h.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.5)}
save(rr3h.tpf, file = "case-only-comparison/par-tpf-comp-rr3-hcsb-HIGH-1082018.RData")
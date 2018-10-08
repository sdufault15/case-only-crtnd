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

# LOW HCSB (50%)
rr1l.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 0.95)}
save(rr1l.tpf, file = "case-only-comparison/par-tpf-comp-rr1-hcsb-LOW-1082018.RData")

rr6l.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.6, lambda.hcsb = 0.95)}
save(rr6l.tpf, file = "case-only-comparison/par-tpf-comp-rr6-hcsb-LOW-1082018.RData")

rr5l.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.5, lambda.hcsb = 0.95)}
save(rr5l.tpf, file = "case-only-comparison/par-tpf-comp-rr5-hcsb-LOW-1082018.RData")

rr4l.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.4, lambda.hcsb = 0.95)}
save(rr4l.tpf, file = "case-only-comparison/par-tpf-comp-rr4-hcsb-LOW-1082018.RData")

rr3l.tpf <- foreach(per = period1, .combine = "rbind") %dopar% {hcsb_tpf_function(data = dta, period = per, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 0.3, lambda.hcsb = 0.95)}
save(rr3l.tpf, file = "case-only-comparison/par-tpf-comp-rr3-hcsb-LOW-1082018.RData")
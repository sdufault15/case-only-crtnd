##### 
# September 21, 2018
# Cluster scripts for evaluating the health care-seeking behavior
# Suzanne Dufault
#####

load("Random10000Allocations.RData")
dta <- subset(Random10000Allocations, select = -X)
source("lib/txtSetFunction.R")
source("lib/gamma-function.R")
library(dplyr)

period1 <- c("03_05", "05_06", "06_07", "07_08", "08_10", "10_11", "11_12", "12_13", "13_14")

# This estimates the differential health care seeking effect at 3 different levels: low, medium, and high

# Low: 5% reduction
hcsb.05 <- gamma_function(dta = dta, period = period1, ncases = 1000, gamma = 0.95)
save(hcsb.05, file = "case-only/hcsb-LOW-09212018.RData")

# Low: 15% reduction
hcsb.15 <- gamma_function(dta = dta, period = period1, ncases = 1000, gamma = 0.85)
save(hcsb.15, file = "case-only/hcsb-MED-09212018.RData")

# Low: 50% reduction
hcsb.50 <- gamma_function(dta = dta, period = period1, ncases = 1000, gamma = 0.50)
save(hcsb.50, file = "case-only/hcsb-HIGH-09212018.RData")

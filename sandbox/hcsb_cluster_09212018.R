##### 
# September 21, 2018
# Cluster scripts for evaluating the case-only approach to estimating hcsb
# Suzanne Dufault
#####

load("Random10000Allocations.RData")
dta <- subset(Random10000Allocations, select = -X)
source("lib/txtSetFunction.R")
source("lib/lambda-gamma-function.R")

period1 <- c("03_05", "05_06", "06_07", "07_08", "08_10", "10_11", "11_12", "12_13", "13_14")

# This is essentially evaluation of health care seeking behavior in the null case - when there has been no forced difference in care seeking. 
# We only have to run this once, since there is not yet an intervention applied.

hcsb1 <- lambda_gamma_function(dta = dta, rrIN = 1, period = period1, ncases = 1000, hcsb = TRUE)
save(hcsb1, file = "case-only/hcsb-NULL-09212018.RData")
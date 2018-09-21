##### 
# March 7, 2018
# Cluster scripts for evaluating the case-only approach to estimating RR
# UPDATE from March 5th - corrected variances
# Suzanne Dufault
#####

load("Random10000Allocations.RData")
dta <- subset(Random10000Allocations, select = -X)
source("lib/txtSetFunction.R")
source("lib/casesOnlyFunction.R")

period1 <- c("03_05", "05_06", "06_07", "07_08", "08_10", "10_11", "11_12", "12_13", "13_14")

case1 <- caseOnlyFunction(dta, rrIN = 1, period = period1, ncases = 1000)
save(case1, file = "case-only/case-only-NULL-03072018.RData")

case6 <- caseOnlyFunction(dta, rrIN = 0.6, period = period1, ncases = 1000)
save(case6, file = "case-only/case-only-6-03072018.RData")

case5 <- caseOnlyFunction(dta, rrIN = 0.5, period = period1, ncases = 1000)
save(case5, file = "case-only/case-only-5-03072018.RData")

case4 <- caseOnlyFunction(dta, rrIN = 0.4, period = period1, ncases = 1000)
save(case4, file = "case-only/case-only-4-03072018.RData")

case3 <- caseOnlyFunction(dta, rrIN = 0.3, period = period1, ncases = 1000)
save(case3, file = "case-only/case-only-3-03072018.RData")
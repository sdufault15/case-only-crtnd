##############################
# Performance Assessment
# Suzanne Dufault
##############################
library(dplyr)
library(here)

# Loading comparison results
load(here("data", "case-only-comparison/all-comp-rr1-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr1-hcsb-MED-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr1-hcsb-LOW-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr1-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/all-comp-rr6-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr6-hcsb-MED-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr6-hcsb-LOW-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr6-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/all-comp-rr5-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr5-hcsb-MED-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr5-hcsb-LOW-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr5-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/all-comp-rr4-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr4-hcsb-MED-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr4-hcsb-LOW-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr4-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/all-comp-rr3-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr3-hcsb-MED-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr3-hcsb-LOW-09222018.RData"))
load(here("data", "case-only-comparison/all-comp-rr3-hcsb1-09222018.RData"))

# Loading test-positive only and OR results
load(here("data", "case-only/all-rr1-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only/all-rr1-hcsb-MED-09222018.RData"))
load(here("data", "case-only/all-rr1-hcsb-LOW-09222018.RData"))
load(here("data", "case-only/all-rr1-hcsb1-09222018.RData"))

load(here("data", "case-only/all-rr6-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only/all-rr6-hcsb-MED-09222018.RData"))
load(here("data", "case-only/all-rr6-hcsb-LOW-09222018.RData"))
load(here("data", "case-only/all-rr6-hcsb1-09222018.RData"))

load(here("data", "case-only/all-rr5-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only/all-rr5-hcsb-MED-09222018.RData"))
load(here("data", "case-only/all-rr5-hcsb-LOW-09222018.RData"))
load(here("data", "case-only/all-rr5-hcsb1-09222018.RData"))

load(here("data", "case-only/all-rr4-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only/all-rr4-hcsb-MED-09222018.RData"))
load(here("data", "case-only/all-rr4-hcsb-LOW-09222018.RData"))
load(here("data", "case-only/all-rr4-hcsb1-09222018.RData"))

load(here("data", "case-only/all-rr3-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only/all-rr3-hcsb-MED-09222018.RData"))
load(here("data", "case-only/all-rr3-hcsb-LOW-09222018.RData"))
load(here("data", "case-only/all-rr3-hcsb1-09222018.RData"))

# Function for test-positive and OR results
est.fun <- function(dta, true.lambda, true.hcsb) {
  bias.lambda.est <-mean(unlist(dta$intervention$lambda.int.est) - true.lambda)
  bias.hcsb.est <- mean(unlist(dta$hcsb$lambda.hcsb.est) - true.hcsb)
  bias.or.est <- mean(unlist(dta$OR$lambda.OR.est) - true.lambda)
  
  cov.lambda.est <- mean(unlist(dta$intervention$coverage.log.lambda.int))
  cov.hcsb.est <- mean(unlist(dta$hcsb$coverage.log.lambda.hcsb))
  cov.or.est <- mean(unlist(dta$OR$coverage.log.lambda.OR))
  
  power.lambda.est <- mean(unlist(dta$intervention$pvals.log.lambda.int) < 0.05)
  power.hcsb.est <- mean(unlist(dta$hcsb$pvals.log.lambda.hcsb) < 0.05)
  power.or.est <- mean(unlist(rr1$OR$signficance.log.lambda.OR))
  
  out <- data.frame(bias.lambda.est, bias.hcsb.est, bias.or.est, 
                    cov.lambda.est, cov.hcsb.est, cov.or.est,
                    power.lambda.est, power.hcsb.est, power.or.est)
  return(out)
}

# Function for comparison results
est.fun.comp <- function(dta, true.lambda) {
  bias.tpf.est <- mean(unlist(lapply(dta, function(x){x$tpf.int.hat}))) - true.lambda
  bias.re.est <- mean(unlist(lapply(dta, function(x){x$re.int.hat}))) - true.lambda
  
  cov.tpf.est <- mean(unlist(lapply(dta, function(x){x$coverage.tpf.int}))) 
  cov.re.est <- mean(unlist(lapply(dta, function(x){x$coverage.log.re.int})))
  
  power.tpf.est <- mean(unlist(lapply(dta, function(x){x$pvals.tpf.int})) < 0.05)
  power.re.est <- mean(unlist(lapply(dta, function(x){x$pvals.log.re.int})) < 0.05)
  
  out <- data.frame(bias.tpf.est, bias.re.est,
                    cov.tpf.est, cov.re.est,
                    power.tpf.est, power.re.est)
  return(out)
}
  
r11 <- est.fun(rr1, 1, 1)
r1l <- est.fun(rr1l, 1, 0.95)
r1m <- est.fun(rr1m, 1, 0.85)
r1h <- est.fun(rr1h, 1, 0.5)

r61 <- est.fun(rr6, 0.6, 1)
r6l <- est.fun(rr6l, 0.6, 0.95)
r6m <- est.fun(rr6m, 0.6, 0.85)
r6h <- est.fun(rr6h, 0.6, 0.5)

r51 <- est.fun(rr5, 0.5, 1)
r5l <- est.fun(rr5l, 0.5, 0.95)
r5m <- est.fun(rr5m, 0.5, 0.85)
r5h <- est.fun(rr5h, 0.5, 0.5)

r41 <- est.fun(rr4, 0.4, 1)
r4l <- est.fun(rr4l, 0.4, 0.95)
r4m <- est.fun(rr4m, 0.4, 0.85)
r4h <- est.fun(rr4h, 0.4, 0.5)

r31 <- est.fun(rr3, 0.3, 1)
r3l <- est.fun(rr3l, 0.3, 0.95)
r3m <- est.fun(rr3m, 0.3, 0.85)
r3h <- est.fun(rr3h, 0.3, 0.5)


r11c <- est.fun.comp(rr1c, 1)
r1lc <- est.fun.comp(rr1lc, 1)
r1mc <- est.fun.comp(rr1mc, 1)
r1hc <- est.fun.comp(rr1hc, 1)

r61c <- est.fun.comp(rr6c, 0.6)
r6lc <- est.fun.comp(rr6lc, 0.6)
r6mc <- est.fun.comp(rr6mc, 0.6)
r6hc <- est.fun.comp(rr6hc, 0.6)

r51c <- est.fun.comp(rr5c, 0.5)
r5lc <- est.fun.comp(rr5lc, 0.5)
r5mc <- est.fun.comp(rr5mc, 0.5)
r5hc <- est.fun.comp(rr5hc, 0.5)

r41c <- est.fun.comp(rr4c, 0.4)
r4lc <- est.fun.comp(rr4lc, 0.4)
r4mc <- est.fun.comp(rr4mc, 0.4)
r4hc <- est.fun.comp(rr4hc, 0.4)

r31c <- est.fun.comp(rr3c, 0.3)
r3lc <- est.fun.comp(rr3lc, 0.3)
r3mc <- est.fun.comp(rr3mc, 0.3)
r3hc <- est.fun.comp(rr3hc, 0.3)

t1 <- bind_rows(r11, r11c, r1l, r1lc, r1m, r1mc, r1h, r1hc,
                r61, r61c, r6l, r6lc, r6m, r6mc, r6h, r6hc,
                r51, r51c, r5l, r5lc, r5m, r5mc, r5h, r5hc,
                r41, r41c, r4l, r4lc, r4m, r4mc, r4h, r4hc,
                r31, r31c, r3l, r3lc, r3m, r3mc, r3h, r3hc)
t1 <- t1 %>% mutate(hcsb = rep(rep(c(1,0.95,0.85,0.5), each = 2), 5),
                    RR = rep(c(1,0.6,0.5,0.4,0.3), each = 8))

bias <- t1 %>% select(RR, hcsb, bias.lambda.est, bias.hcsb.est, bias.or.est, bias.tpf.est, bias.re.est) %>% gather("method", "bias", 3:7)
coverage <- t1 %>% select(RR, hcsb, cov.lambda.est, cov.hcsb.est, cov.or.est, cov.tpf.est, cov.re.est) %>% gather("method", "coverage", 3:7)
power <- t1 %>% select(RR, hcsb, power.lambda.est, power.hcsb.est, power.or.est, power.tpf.est, power.re.est) %>% gather("method", "power", 3:7)

performance <- list(bias = bias, coverage = coverage, power = power)
save(performance, file = here("data", "performance-metrics.RData"))

##################################### 
# Constructing the Performance Plots
# Suzanne M. Dufault
#####################################

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
myColors <- brewer.pal(8, 'Set2')

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

###############
# Performance Estimates
###############

est.fun <- function(dta, true.lambda, true.hcsb){
  bias.lambda.est <- mean(unlist(dta$intervention$lambda.int.est) - true.lambda)
  bias.hcsb.est <- mean(unlist(dta$hcsb$lambda.hcsb.est) - true.hcsb)
  bias.or.est <- mean(unlist(dta$OR$lambda.OR.est) - true.lambda)
  
  cov.lambda.est <- mean(unlist(dta$intervention$coverage.log.lambda.int))
  cov.hcsb.est <- mean(unlist(dta$hcsb$coverage.log.lambda.hcsb))
  cov.or.est <- mean(unlist(dta$OR$coverage.log.lambda.OR))
  
  power.lambda.est <- mean(unlist(dta$intervention$pvals.log.lambda.int) < 0.05)
  power.hcsb.est <- mean(unlist(dta$hcsb$pvals.log.lambda.hcsb) < 0.05)
  power.or.est <- mean(unlist(rr1$OR$signficance.log.lambda.OR))
  
  out <- data.frame(bias.lambda.est, bias.hcsb.est, bias.or.est, cov.lambda.est, cov.hcsb.est, cov.or.est, power.lambda.est, power.hcsb.est, power.or.est)
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

t1 <- bind_rows(r11, r1l, r1m, r1h,
          r61, r6l, r6m, r6h,
          r51, r5l, r5m, r5h,
          r41, r4l, r4m, r4h,
          r31, r3l, r3m, r3h)
t1 <- t1 %>% mutate(hcsb = rep(c(1,0.95,0.85,0.5), 5),
                    RR = rep(c(1,0.6,0.5,0.4,0.3), each = 4))

t1a <- t1 %>% select(RR, hcsb, bias.lambda.est, bias.hcsb.est, bias.or.est) %>% gather("method", "bias", 3:5)
t1b <- t1 %>% select(RR, hcsb, cov.lambda.est, cov.hcsb.est, cov.or.est) %>% gather("method", "coverage", 3:5)
t1c <- t1 %>% select(RR, hcsb, power.lambda.est, power.hcsb.est, power.or.est) %>% gather("method", "coverage", 3:5)

t1a %>% filter(method != 'bias.hcsb.est', RR == 0.6) %>% ggplot(aes(x = hcsb, y = bias, shape = method, color = method, group = method)) + 
  ylim(-0.5, 0.25) + 
  xlab("Health-Care--Seeking Behavior\n(Low to High)") +
  ylab("Bias") + 
  ggtitle("PLACE HOLDER: Bias of Different Estimation Methods", subtitle = "RR = 0.6") + 
  geom_point(size = 4) + 
  geom_line(size = 1) + 
  geom_hline(yintercept = 0, lty = 2, size = 1) + 
  scale_x_reverse() + 
  scale_color_manual("Method", values = myColors, labels = c("Test-Positive Only", "Aggregate Odds Ratio")) + 
  scale_shape_manual("Method", values = c(16,17), labels = c("Test-Positive Only", "Aggregate Odds Ratio")) +
  theme_pubclean() 
ggsave(here("graphs", "bias-comparison.png"), device = 'png', width = 6, height = 4, units = "in")

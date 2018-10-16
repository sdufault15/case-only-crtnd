##############################
# Performance Assessment
# Suzanne Dufault
##############################
library(dplyr)
library(tidyr)
library(here)

# Loading comparison results
load(here("data", "case-only-comparison/par-tpf-comp-rr1-hcsb-HIGH-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr1-hcsb-MED-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr1-hcsb-LOW-1082018.RData"))
load(here("data", "case-only-comparison/all-comp-rr1-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/par-tpf-comp-rr6-hcsb-HIGH-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr6-hcsb-MED-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr6-hcsb-LOW-1082018.RData"))
load(here("data", "case-only-comparison/all-comp-rr6-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/par-tpf-comp-rr5-hcsb-HIGH-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr5-hcsb-MED-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr5-hcsb-LOW-1082018.RData"))
#load(here("data", "case-only-comparison/all-comp-rr5-hcsb1-09222018.RData"))

#load(here("data", "case-only-comparison/par-tpf-comp-rr4-hcsb-HIGH-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr4-hcsb-MED-1082018.RData"))
#load(here("data", "case-only-comparison/par-tpf-comp-rr4-hcsb-LOW-1082018.RData"))
#load(here("data", "case-only-comparison/all-comp-rr4-hcsb1-09222018.RData"))

#load(here("data", "case-only-comparison/par-tpf-comp-rr3-hcsb-HIGH-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr3-hcsb-MED-1082018.RData"))
#load(here("data", "case-only-comparison/par-tpf-comp-rr3-hcsb-LOW-1082018.RData"))
#load(here("data", "case-only-comparison/all-comp-rr3-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/re-comp-rr1-hcsb-HIGH-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr1-hcsb-MED-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr1-hcsb-LOW-10112018.RData"))
load(here("data", "case-only-comparison/all-comp-rr1-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/re-comp-rr6-hcsb-HIGH-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr6-hcsb-MED-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr6-hcsb-LOW-10112018.RData"))
load(here("data", "case-only-comparison/all-comp-rr6-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/re-comp-rr5-hcsb-HIGH-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr5-hcsb-MED-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr5-hcsb-LOW-10112018.RData"))
#load(here("data", "case-only-comparison/all-comp-rr5-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/re-comp-rr4-hcsb-HIGH-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr4-hcsb-MED-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr4-hcsb-LOW-10112018.RData"))
#load(here("data", "case-only-comparison/all-comp-rr4-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/re-comp-rr3-hcsb-HIGH-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr3-hcsb-MED-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr3-hcsb-LOW-10112018.RData"))
#load(here("data", "case-only-comparison/all-comp-rr3-hcsb1-09222018.RData"))

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
  bias.lambda.est <- unlist(dta$intervention$lambda.int.est) - true.lambda
  bias.hcsb.est <- unlist(dta$hcsb$lambda.hcsb.est) - true.hcsb
  bias.or.est <- unlist(dta$OR$lambda.OR.est) - true.lambda
  
  cov.lambda.est <- unlist(dta$intervention$coverage.log.lambda.int)
  cov.hcsb.est <- unlist(dta$hcsb$coverage.log.lambda.hcsb)
  cov.or.est <- unlist(dta$OR$coverage.log.lambda.OR)
  
  power.lambda.est <- unlist(dta$intervention$pvals.log.lambda.int) < 0.05
  power.hcsb.est <- unlist(dta$hcsb$pvals.log.lambda.hcsb) < 0.05
  power.or.est <- unlist(rr1$OR$signficance.log.lambda.OR)
  
  out <- data.frame(bias.lambda.est, bias.hcsb.est, bias.or.est, 
                    cov.lambda.est, cov.hcsb.est, cov.or.est,
                    power.lambda.est, power.hcsb.est, power.or.est)
  return(out)
}

# ERROR IN THE RE RESULTS, NEED TO RE-DO COVERAGE ESTIMATION

# Function for comparison results
est.fun.comp <- function(dta.in.re, dta.in.tpf, true.lambda) {

  bias.tpf.est <- unlist(lapply(dta.in.tpf, function(x){x$tpf.int.hat})) - true.lambda
  bias.re.est <- unlist(lapply(dta.in.re, function(x){x$re.int.hat})) - true.lambda
  
  cov.tpf.est <- unlist(lapply(dta.in.tpf, function(x){x$coverage.tpf.int})) 
  cov.re.est <- unlist(lapply(dta.in.re, function(x){(as.numeric(log(true.lambda) >= log(x$re.int.hat) - 1.96*sqrt(x$var.log.re.int)) & (log(true.lambda) <= log(x$re.int.hat) + 1.96*sqrt(x$var.log.re.int)))}))
  
  power.tpf.est <- unlist(lapply(dta.in.tpf, function(x){x$pvals.tpf.int})) < 0.05
  power.re.est <- unlist(lapply(dta.in.re, function(x){x$pvals.log.re.int})) < 0.05
  
  out <- data.frame(bias.tpf.est, bias.re.est,
                    cov.tpf.est, cov.re.est,
                    power.tpf.est, power.re.est)
  return(out)
}

# For RR = 0.6, HCSB = none, low, medium, high
r61 <- est.fun(rr6, 0.6, 1)
r6l <- est.fun(rr6l, 0.6, 0.95)
r6m <- est.fun(rr6m, 0.6, 0.85)
r6h <- est.fun(rr6h, 0.6, 0.5)

r6hc <- est.fun.comp(rr6h.re, rr6h.tpf, 0.6)
r6mc <- est.fun.comp(rr6m.re, rr6m.tpf, 0.6)
r6lc <- est.fun.comp(rr6l.re, rr6l.tpf, 0.6)

total.a <- bind_rows(r61, r6l, r6m, r6h)
total.a$hcsb <- rep(c(1,0.95,0.85,0.5), each = 90000)
total.b <- bind_rows(r6lc, r6mc, r6hc)
total.b$hcsb <- rep(c(0.95, 0.85, 0.5), each = 90000)

total.a <- total.a %>% gather("method", "value", 1:9)
total.b <- total.b %>% gather("method", "value", 1:6)

all <- bind_rows(total.a, total.b) %>% arrange(hcsb)
all <- all %>% mutate(hcsb = ifelse(hcsb == 1, "None", ifelse(hcsb == 0.95, "Low", ifelse(hcsb == 0.85, "Medium", "High"))))
#all <- all %>% mutate(method = ifelse("bias.lambda.est", "Test-Positive Only", 
#                                      ifelse("bias.or.est", "Odds Ratio", 
#                                             ifelse("bias.tpf.est", "Test-Positive Fraction", "Random Effects"))))

library(RColorBrewer)
myColors <- c("#d1b086", "#58ACB9", "#C0C3C3", "#50603F", "#969892")
all %>% filter(method %in% grep("hcsb", grep("bias", all$method, value = TRUE), invert = TRUE, value = TRUE)) %>% 
  ggplot(aes(x = hcsb, y = value + 0.6, color = method)) + 
  scale_x_discrete(limits = c("None", "Low", "Medium", "High")) + 
  ylim(0,1.75) + 
  theme_minimal() + 
  theme(text = element_text(size = 20), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 18), axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 22)) + 
  scale_color_manual("Estimation Method", values = myColors[1:4],labels = c("Test-Positive Only", "Odds Ratio", "Random Effects", "Test-Positive Fraction")) + 
  geom_boxplot(width = 0.4, position = position_dodge(0.45), lwd = 1) + 
  annotate(geom = 'text', x = 0.6, y = 0.635, label = "Truth", color = myColors[5]) + 
  geom_hline(yintercept = 0.6, lty = 2, col = myColors[5]) +
  ylab("Estimated RR of Intervention\nRR = 0.6") + 
  xlab("Differential Health-Care-Seeking Behavior")
ggsave(here("graphs", "bias-comparison-6.png"), device = "png", height = 6, width = 12, units = "in")

r6s <- bind_rows(apply(r61, 2, mean), apply(r6l, 2, mean), apply(r6m, 2, mean), apply(r6h, 2, mean)) 
r6s$hcsb <- c(1, 0.95, 0.85, 0.5)
r6ss <- bind_rows(apply(r6lc, 2, mean), apply(r6mc, 2, mean), apply(r6hc, 2, mean)) 
r6ss$hcsb <- c(0.95, 0.85, 0.5)

r6sd <- bind_rows(apply(r61, 2, sd), apply(r6l, 2, sd), apply(r6m, 2, sd), apply(r6h, 2, sd)) 
#names(r6sd) <- paste0("sd.", names(r6sd))
r6sd$hcsb <- c(1, 0.95, 0.85, 0.5)
r6ssd <- bind_rows(apply(r6lc, 2, sd), apply(r6mc, 2, sd), apply(r6hc, 2, sd))
#names(r6ssd) <- paste0("sd.", names(r6ssd))
r6ssd$hcsb <- c(0.95, 0.85, 0.5)

r6s <- full_join(r6ss, r6s, by = "hcsb") 
r6sd <- full_join(r6ssd, r6sd, by = "hcsb")

r6.long <- r6s %>% gather("method", "value", c(1:6,8:16))
r6s.long <- r6sd %>% gather("method", "sd", c(1:6, 8:16))
r6.long <- full_join(r6.long, r6s.long, by = c("hcsb", "method"))

pd <- position_dodge(0.1) # move them .05 to the left and right
r6.long %>% filter(method %in% grep("hcsb", grep("bias", r6.long$method, value = TRUE), invert= TRUE, value = TRUE)) %>%  ggplot(aes(x = hcsb, y = value, color = method, shape = method, group = method)) +
  geom_point(size = 4) + 
  scale_x_reverse() +
  geom_line(size = 1) + 
  #geom_boxplot(position = 'dodge2') + 
  #geom_errorbar(aes(ymin = value - 1.96*(sd/90000^2), ymax = value + 1.96*(sd/90000^2), width = .1)) +
  theme_minimal()


r11 <- est.fun(rr1, 1, 1)
r1l <- est.fun(rr1l, 1, 0.95)
r1m <- est.fun(rr1m, 1, 0.85)
r1h <- est.fun(rr1h, 1, 0.5)

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


r1hc <- est.fun.comp.tpf(rr1h.tpf, 1)
r1mc <- est.fun.comp.tpf(rr1m.tpf, 1)
r1lc <- est.fun.comp.tpf(rr1l.tpf, 1)

r5hc <- est.fun.comp.tpf(rr5h.tpf, 0.5)
r5mc <- est.fun.comp.tpf(rr5m.tpf, 0.5)
r5lc <- est.fun.comp.tpf(rr5l.tpf, 0.5)

r4hc <- est.fun.comp.tpf(rr4h.tpf, 0.4)
r4mc <- est.fun.comp.tpf(rr4m.tpf, 0.4)
r4lc <- est.fun.comp.tpf(rr4l.tpf, 0.4)

r3hc <- est.fun.comp.tpf(rr3h.tpf, 0.3)
r3mc <- est.fun.comp.tpf(rr3m.tpf, 0.3)
r3lc <- est.fun.comp.tpf(rr3l.tpf, 0.3)

t1 <- bind_cols(bind_rows(r11, r1l, r1m, r1h,
                          r61, r6l, r6m, r6h,
                          r51, r5l, r5m, r5h,
                          r41, r4l, r4m, r4h,
                          r31, r3l, r3m, r3h), 
                bind_rows(r11c, r1lc, r1mc, r1hc,
                          r61c, r6lc, r6mc, r6hc,
                          r51c, r5lc, r5mc, r5hc,
                          r41c, r4lc, r4mc, r4hc,
                          r31c, r3lc, r3mc, r3hc))

# For now
t1 <- bind_cols(bind_rows(r11, r1l, r1m, r1h#,
                          #r61, r6l, r6m, r6h,
                          #r51, r5l, r5m, r5h,
                          #r41, r4l, r4m, r4h,
                          #r31, r3l, r3m, r3h
                          ), 
                bind_rows(r11c, r1lc, r1mc, r1hc #,
                          #r61c, r6lc, r6mc, r6hc,
                          #r51c, r5lc, r5mc, r5hc,
                          #r41c, r4lc, r4mc, r4hc,
                          #r31c, r3lc, r3mc, r3hc
                          ))
t1 <- t1 %>% mutate(hcsb = c(1,0.95,0.85,0.5), RR = rep(1, 4))
#t1 <- t1 %>% mutate(hcsb = rep(rep(c(1,0.95,0.85,0.5), each = 2), 5),
#                    RR = rep(c(1,0.6,0.5,0.4,0.3), each = 8))

bias <- t1 %>% select_at(vars(c(RR, hcsb, starts_with("bias")))) %>% gather("method", "bias", 3:7)
coverage <- t1 %>% select(RR, hcsb, cov.lambda.est, cov.hcsb.est, cov.or.est, cov.tpf.est, cov.re.est) %>% gather("method", "coverage", 3:7)
power <- t1 %>% select(RR, hcsb, power.lambda.est, power.hcsb.est, power.or.est, power.tpf.est, power.re.est) %>% gather("method", "power", 3:7)

performance <- list(bias = bias, coverage = coverage, power = power)
save(performance, file = here("data", "performance-metrics.RData"))

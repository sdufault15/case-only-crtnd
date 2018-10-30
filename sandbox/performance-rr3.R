library(here)
library(tidyverse)

source(here("lib", "performance-functions.R"))

load(here("data", "case-only-comparison/par-tpf-comp-rr3-hcsb-HIGH-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr3-hcsb-MED-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr3-hcsb-LOW-1082018.RData"))

load(here("data", "case-only-comparison/re-comp-rr3-hcsb-HIGH-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr3-hcsb-MED-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr3-hcsb-LOW-10112018.RData"))

load(here("data", "case-only/all-rr3-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only/all-rr3-hcsb-MED-09222018.RData"))
load(here("data", "case-only/all-rr3-hcsb-LOW-09222018.RData"))
load(here("data", "case-only/all-rr3-hcsb1-09222018.RData"))

# Getting the no HCSB results from previous paper
load("~/Box Sync/Research/TND_CRT_Paper1_2017/CR_TND/dec17/r_est3_12222017.RData")
load("~/Box Sync/Research/TND_CRT_Paper1_2017/CR_TND/dec17/random3tTest1_12192017.Rdata")
rr31.re <- r.est3.1
rr31.tpf <- t30r.2.1
rm(r.est3.1, t30r.2.1)

# For RR = 0.3, HCSB = none, low, medium, high
r31 <- est.fun(rr3, 0.3, 1)
r3l <- est.fun(rr3l, 0.3, 0.95)
r3m <- est.fun(rr3m, 0.3, 0.85)
r3h <- est.fun(rr3h, 0.3, 0.5)

r3hc <- est.fun.comp(rr3h.re, rr3h.tpf, 0.3)
r3mc <- est.fun.comp(rr3m.re, rr3m.tpf, 0.3)
r3lc <- est.fun.comp(rr3l.re, rr3l.tpf, 0.3)
r31c <- old.performance.function(rr31.re, rr31.tpf, 0.3)

total.a <- bind_rows(r31, r3l, r3m, r3h)
total.a$hcsb <- rep(c(1,0.95,0.85,0.5), each = 90000)
total.b <- bind_rows(r31c, r3lc, r3mc, r3hc)
total.b$hcsb <- rep(c(1,0.95, 0.85, 0.5), each = 90000)

total.a <- total.a %>% gather("method", "value", 1:9)
total.b <- total.b %>% gather("method", "value", 1:6)

all <- bind_rows(total.a, total.b) %>% arrange(hcsb)
all <- all %>% mutate(hcsb = ifelse(hcsb == 1, "None", ifelse(hcsb == 0.95, "Low", ifelse(hcsb == 0.85, "Medium", "High"))))

library(RColorBrewer)
myColors <- c("#d1b086", "#58ACB9", "#C0C3C3", "#50603F", "#969892")
all %>% filter(method %in% grep("hcsb", grep("bias", all$method, value = TRUE), invert = TRUE, value = TRUE), 
               method %in% grep("tpf", all$method, value = TRUE, invert = TRUE)) %>% 
  ggplot(aes(x = hcsb, y = value + 0.3, color = method)) + 
  scale_x_discrete(limits = c("None", "Low", "Medium", "High")) + 
  ylim(0,1) + 
  theme_classic() + 
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 24, margin = margin(10,0,0,0)), 
        axis.title.y = element_text(size = 22, margin = margin(0,10,0,0))) + 
  scale_color_manual("Estimation Method", values = myColors[1:3],labels = c("Test-Positive Only", "Odds Ratio", "Mixed Effects")) + 
  geom_boxplot(width = 0.4, position = position_dodge(0.45), lwd = 1) + 
  annotate(geom = 'text', x = 0.6, y = 0.335, label = "Truth", color = myColors[5]) + 
  geom_hline(yintercept = 0.3, lty = 2, col = myColors[5]) +
  ylab("Estimated RR of Intervention\nRR = 0.3") + 
  xlab("Differential Health-Care-Seeking Behavior")
ggsave(here("graphs", "bias-comparison-3.png"), device = "png", height = 6, width = 12, units = "in")

# For Coverage and Power Plots
all$RR = 0.3
save(all, file = here("data", "performance-3.RData"))

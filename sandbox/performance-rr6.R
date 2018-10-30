library(here)
library(tidyverse)

source(here("lib", "performance-functions.R"))

load(here("data", "case-only-comparison/par-tpf-comp-rr6-hcsb-HIGH-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr6-hcsb-MED-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr6-hcsb-LOW-1082018.RData"))
load(here("data", "case-only-comparison/all-comp-rr6-hcsb1-09222018.RData"))

load(here("data", "case-only-comparison/re-comp-rr6-hcsb-HIGH-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr6-hcsb-MED-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr6-hcsb-LOW-10112018.RData"))
load(here("data", "case-only-comparison/all-comp-rr6-hcsb1-09222018.RData"))

load(here("data", "case-only/all-rr6-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only/all-rr6-hcsb-MED-09222018.RData"))
load(here("data", "case-only/all-rr6-hcsb-LOW-09222018.RData"))
load(here("data", "case-only/all-rr6-hcsb1-09222018.RData"))

load("~/Box Sync/Research/TND_CRT_Paper1_2017/CR_TND/dec17/r_est6_12222017.RData")
load("~/Box Sync/Research/TND_CRT_Paper1_2017/CR_TND/dec17/random6tTest1_12192017.Rdata")
rr61.re <- r.est6.1
rr61.tpf <- t60r.2.1
rm(r.est6.1, t60r.2.1)

# For RR = 0.6, HCSB = none, low, medium, high
r61 <- est.fun(rr6, 0.6, 1)
r6l <- est.fun(rr6l, 0.6, 0.95)
r6m <- est.fun(rr6m, 0.6, 0.85)
r6h <- est.fun(rr6h, 0.6, 0.5)

r6hc <- est.fun.comp(rr6h.re, rr6h.tpf, 0.6)
r6mc <- est.fun.comp(rr6m.re, rr6m.tpf, 0.6)
r6lc <- est.fun.comp(rr6l.re, rr6l.tpf, 0.6)
r61c <- old.performance.function(rr61.re, rr61.tpf, 0.6)

total.a <- bind_rows(r61, r6l, r6m, r6h)
total.a$hcsb <- rep(c(1,0.95,0.85,0.5), each = 90000)
total.b <- bind_rows(r61c, r6lc, r6mc, r6hc)
total.b$hcsb <- rep(c(1,0.95, 0.85, 0.5), each = 90000)

total.a <- total.a %>% gather("method", "value", 1:9)
total.b <- total.b %>% gather("method", "value", 1:6)

all <- bind_rows(total.a, total.b) %>% arrange(hcsb)
all <- all %>% mutate(hcsb = ifelse(hcsb == 1, "None", ifelse(hcsb == 0.95, "Low", ifelse(hcsb == 0.85, "Medium", "High"))))

library(RColorBrewer)
myColors <- c("#d1b086", "#58ACB9", "#C0C3C3", "#50603F", "#969892")
all %>% filter(method %in% grep("hcsb", grep("bias", all$method, value = TRUE), invert = TRUE, value = TRUE),
               method %in% grep("tpf", all$method, invert = TRUE, value = TRUE)) %>% 
  ggplot(aes(x = hcsb, y = value + 0.6, color = method)) + 
  scale_x_discrete(limits = c("None", "Low", "Medium", "High")) + 
  ylim(0,1.75) + 
  theme_classic() + 
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 24, margin = margin(10,0,0,0)), 
        axis.title.y = element_text(size = 22, margin = margin(0,10,0,0))) + 
  scale_color_manual("Estimation Method", values = myColors[1:3],labels = c("Test-Positive Only", "Odds Ratio", "Random Effects", "Test-Positive Fraction")) + 
  geom_boxplot(width = 0.4, position = position_dodge(0.45), lwd = 1) + 
  annotate(geom = 'text', x = 0.6, y = 0.635, label = "Truth", color = myColors[5]) + 
  geom_hline(yintercept = 0.6, lty = 2, col = myColors[5]) +
  ylab("Estimated RR of Intervention\nRR = 0.6") + 
  xlab("Differential Health-Care-Seeking Behavior")
ggsave(here("graphs", "bias-comparison-6.png"), device = "png", height = 6, width = 12, units = "in")

# For Coverage and Power Plots
all$RR = 0.6
save(all, file = here("data", "performance-6.RData"))

library(here)
library(tidyverse)

source(here("lib", "performance-functions.R"))

load(here("data", "case-only-comparison/par-tpf-comp-rr5-hcsb-HIGH-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr5-hcsb-MED-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr5-hcsb-LOW-1082018.RData"))
#load(here("data", "case-only-comparison/par-tpf-comp-rr5-hcsb-1082018.RData"))

load(here("data", "case-only-comparison/re-comp-rr5-hcsb-HIGH-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr5-hcsb-MED-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr5-hcsb-LOW-10112018.RData"))
#load(here("data", "case-only-comparison/re-comp-rr5-hcsb1-09222018.RData"))

load(here("data", "case-only/all-rr5-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only/all-rr5-hcsb-MED-09222018.RData"))
load(here("data", "case-only/all-rr5-hcsb-LOW-09222018.RData"))
load(here("data", "case-only/all-rr5-hcsb1-09222018.RData"))

# Getting the no HCSB results from previous paper
load("~/Box Sync/Research/TND_CRT_Paper1_2017/CR_TND/dec17/r_est5_12222017.RData")
load("~/Box Sync/Research/TND_CRT_Paper1_2017/CR_TND/dec17/random5tTest1_12192017.Rdata")
rr51.re <- r.est5.1
rr51.tpf <- t50r.2.1
rm(r.est5.1, t50r.2.1)

# For RR = 0.5, HCSB = none, low, medium, high
r51 <- est.fun(rr5, 0.5, 1)
r5l <- est.fun(rr5l, 0.5, 0.95)
r5m <- est.fun(rr5m, 0.5, 0.85)
r5h <- est.fun(rr5h, 0.5, 0.5)

r5hc <- est.fun.comp(rr5h.re, rr5h.tpf, 0.5)
r5mc <- est.fun.comp(rr5m.re, rr5m.tpf, 0.5)
r5lc <- est.fun.comp(rr5l.re, rr5l.tpf, 0.5)
r51c <- old.performance.function(rr51.re, rr51.tpf, 0.5)

total.a <- bind_rows(r51, r5l, r5m, r5h)
total.a$hcsb <- rep(c(1,0.95,0.85,0.5), each = 90000)
total.b <- bind_rows(r51c, r5lc, r5mc, r5hc)
total.b$hcsb <- rep(c(1,0.95, 0.85, 0.5), each = 90000)

total.a <- total.a %>% gather("method", "value", 1:9)
total.b <- total.b %>% gather("method", "value", 1:6)

all <- bind_rows(total.a, total.b) %>% arrange(hcsb)
all <- all %>% mutate(hcsb = ifelse(hcsb == 1, "None", ifelse(hcsb == 0.95, "Low", ifelse(hcsb == 0.85, "Medium", "High"))))

library(RColorBrewer)
myColors <- c("#d1b086", "#58ACB9", "#C0C3C3", "#50603F", "#969892")
all %>% filter(method %in% grep("hcsb", grep("bias", all$method, value = TRUE), invert = TRUE, value = TRUE),
               method %in% grep("tpf", all$method, invert = TRUE, value = TRUE)) %>% 
  ggplot(aes(x = hcsb, y = value + 0.5, color = method)) + 
  scale_x_discrete(limits = c("None", "Low", "Medium", "High")) + 
  ylim(0,1.5) + 
  theme_classic() + 
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 24, margin = margin(10,0,0,0)), 
        axis.title.y = element_text(size = 22, margin = margin(0,10,0,0))) + 
  scale_color_manual("Estimation Method", values = myColors[1:3],labels = c("Test-Positive Only", "Odds Ratio", "Random Effects")) + 
  geom_boxplot(width = 0.4, position = position_dodge(0.45), lwd = 1) + 
  annotate(geom = 'text', x = 0.6, y = 0.535, label = "Truth", color = myColors[5]) + 
  geom_hline(yintercept = 0.5, lty = 2, col = myColors[5]) +
  ylab("Estimated RR of Intervention\nRR = 0.5") + 
  xlab("Differential Health-Care-Seeking Behavior")
ggsave(here("graphs", "bias-comparison-5.png"), device = "png", height = 6, width = 12, units = "in")

# For Coverage and Power Plots
all$RR = 0.5
save(all, file = here("data", "performance-5.RData"))

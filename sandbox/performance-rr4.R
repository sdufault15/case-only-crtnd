library(here)
library(tidyverse)

source(here("lib", "performance-functions.R"))

load(here("data", "case-only-comparison/par-tpf-comp-rr4-hcsb-HIGH-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr4-hcsb-MED-1082018.RData"))
load(here("data", "case-only-comparison/par-tpf-comp-rr4-hcsb-LOW-1082018.RData"))

load(here("data", "case-only-comparison/re-comp-rr4-hcsb-HIGH-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr4-hcsb-MED-10112018.RData"))
load(here("data", "case-only-comparison/re-comp-rr4-hcsb-LOW-10112018.RData"))

load(here("data", "case-only/all-rr4-hcsb-HIGH-09222018.RData"))
load(here("data", "case-only/all-rr4-hcsb-MED-09222018.RData"))
load(here("data", "case-only/all-rr4-hcsb-LOW-09222018.RData"))
load(here("data", "case-only/all-rr4-hcsb1-09222018.RData"))

# Getting the no HCSB results from previous paper
load("~/Box Sync/Research/TND_CRT_Paper1_2017/CR_TND/dec17/r_est4_12222017.RData")
load("~/Box Sync/Research/TND_CRT_Paper1_2017/CR_TND/dec17/random4tTest1_12192017.Rdata")
rr41.re <- r.est4.1
rr41.tpf <- t40r.2.1
rm(r.est4.1, t40r.2.1)

# For RR = 0.4, HCSB = none, low, medium, high
r41 <- est.fun(rr4, 0.4, 1)
r4l <- est.fun(rr4l, 0.4, 0.95)
r4m <- est.fun(rr4m, 0.4, 0.85)
r4h <- est.fun(rr4h, 0.4, 0.5)

r4hc <- est.fun.comp(rr4h.re, rr4h.tpf, 0.4)
r4mc <- est.fun.comp(rr4m.re, rr4m.tpf, 0.4)
r4lc <- est.fun.comp(rr4l.re, rr4l.tpf, 0.4)
r41c <- old.performance.function(rr41.re, rr41.tpf, 0.4)

total.a <- bind_rows(r41, r4l, r4m, r4h)
total.a$hcsb <- rep(c(1,0.95,0.85,0.5), each = 90000)
total.b <- bind_rows(r41c, r4lc, r4mc, r4hc)
total.b$hcsb <- rep(c(1,0.95, 0.85, 0.5), each = 90000)

total.a <- total.a %>% gather("method", "value", 1:9)
total.b <- total.b %>% gather("method", "value", 1:6)

all <- bind_rows(total.a, total.b) %>% arrange(hcsb)
all <- all %>% mutate(hcsb = ifelse(hcsb == 1, "None", ifelse(hcsb == 0.95, "Low", ifelse(hcsb == 0.85, "Medium", "High"))))

library(RColorBrewer)
myColors <- c("#d1b086", "#58ACB9", "#C0C3C3", "#50603F", "#969892")
all %>% filter(method %in% grep("hcsb", grep("bias", all$method, value = TRUE), invert = TRUE, value = TRUE),
               method %in% grep("tpf", all$method, value = TRUE, invert = TRUE)) %>% 
  ggplot(aes(x = hcsb, y = value + 0.4, color = method)) + 
  scale_x_discrete(limits = c("None", "Low", "Medium", "High")) + 
  ylim(0,1.25) + 
  theme_classic() + 
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 24, margin = margin(10,0,0,0)), 
        axis.title.y = element_text(size = 22, margin = margin(0,10,0,0))) + 
  scale_color_manual("Estimation Method", values = myColors[1:3],labels = c("Test-Positive Only", "Odds Ratio", "Random Effects")) + 
  geom_boxplot(width = 0.4, position = position_dodge(0.45), lwd = 1) + 
  annotate(geom = 'text', x = 0.6, y = 0.435, label = "Truth", color = myColors[5]) + 
  geom_hline(yintercept = 0.4, lty = 2, col = myColors[5]) +
  ylab("Estimated RR of Intervention\nRR = 0.4") + 
  xlab("Differential Health-Care-Seeking Behavior")
ggsave(here("graphs", "bias-comparison-4.png"), device = "png", height = 6, width = 12, units = "in")


# For Coverage and Power Plots
all$RR = 0.4
save(all, file = here("data", "performance-4.RData"))

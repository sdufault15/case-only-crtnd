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

load(here("data", "performance-metrics.RData"))

performance$bias %>% filter(method != 'bias.hcsb.est', RR == 0.6) %>% ggplot(aes(x = hcsb, y = bias, shape = method, color = method, group = method)) + 
  ylim(-0.5, 0.25) + 
  xlab("Health-Care--Seeking Behavior\n(Low to High)") +
  ylab("Bias") + 
  ggtitle("PLACE HOLDER: Bias of Different Estimation Methods", subtitle = "RR = 0.6") + 
  geom_point(size = 4) + 
  geom_line(size = 1) + 
  geom_hline(yintercept = 0, lty = 2, size = 1) + 
  scale_x_reverse() + 
  scale_color_manual("Method", values = myColors, labels = c("Test-Positive Only", "Aggregate Odds Ratio", "Test-Positive Fraction", "Mixed Effects")) + 
  scale_shape_manual("Method", values = c(16,17,18,15), labels = c("Test-Positive Only", "Aggregate Odds Ratio", "Test-Positive Fraction", "Mixed Effects")) +
  theme_pubclean() 
ggsave(here("graphs", "bias-comparison.png"), device = 'png', width = 6, height = 4, units = "in")

performance$coverage %>% filter(method != 'cov.hcsb.est', RR == 0.6) %>% ggplot(aes(x = hcsb, y = coverage, shape = method, color = method, group = method)) + 
  ylim(0, 1) + 
  xlab("Health-Care--Seeking Behavior\n(Low to High)") +
  ylab("Coverage") + 
  ggtitle("PLACE HOLDER: Coverage of Different Estimation Methods", subtitle = "RR = 0.6") + 
  geom_point(size = 4) + 
  geom_line(size = 1) + 
  geom_hline(yintercept = 0.95, lty = 2, size = 1) + 
  scale_x_reverse() + 
  scale_color_manual("Method", values = myColors, labels = c("Test-Positive Only", "Aggregate Odds Ratio", "Test-Positive Fraction", "Mixed Effects")) + 
  scale_shape_manual("Method", values = c(16,17,18,15), labels = c("Test-Positive Only", "Aggregate Odds Ratio", "Test-Positive Fraction", "Mixed Effects")) +
  theme_pubclean() 
ggsave(here("graphs", "coverage-comparison.png"), device = 'png', width = 6, height = 4, units = "in")

performance$power %>% filter(method != 'power.hcsb.est', RR == 0.6) %>% ggplot(aes(x = hcsb, y = power, shape = method, color = method, group = method)) + 
  ylim(0, 1) + 
  xlab("Health-Care--Seeking Behavior\n(Low to High)") +
  ylab("Power") + 
  ggtitle("PLACE HOLDER: Power of Different Estimation Methods", subtitle = "RR = 0.6") + 
  geom_point(size = 4) + 
  geom_line(size = 1) + 
  geom_hline(yintercept = 0.8, lty = 2, size = 1) + 
  scale_x_reverse() + 
  scale_color_manual("Method", values = myColors, labels = c("Test-Positive Only", "Aggregate Odds Ratio", "Test-Positive Fraction", "Mixed Effects")) + 
  scale_shape_manual("Method", values = c(16,17,18,15), labels = c("Test-Positive Only", "Aggregate Odds Ratio", "Test-Positive Fraction", "Mixed Effects")) +
  theme_pubclean() 
ggsave(here("graphs", "power-comparison.png"), device = 'png', width = 6, height = 4, units = "in")

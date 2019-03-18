library(here)
library(tidyverse)
library(RColorBrewer)

load(here("data", "performance-3.RData"))
p3 <- all

load(here("data", "performance-4.RData"))
p4 <- all

load(here("data", "performance-5.RData"))
p5 <- all

load(here("data", "performance-6.RData"))
p6 <- all

rm(all)

# Coverage

cov3 <- p3 %>% 
  filter(method %in% grep("hcsb", grep("cov", p3$method, value = TRUE), invert = TRUE, value = TRUE)) %>%
  group_by(hcsb, method, RR) %>%
  mutate(value = mean(value)) %>% 
  distinct()

cov4 <- p4 %>% 
  filter(method %in% grep("hcsb", grep("cov", p3$method, value = TRUE), invert = TRUE, value = TRUE)) %>%
  group_by(hcsb, method, RR) %>%
  mutate(value = mean(value)) %>% 
  distinct()

cov5 <- p5 %>% 
  filter(method %in% grep("hcsb", grep("cov", p3$method, value = TRUE), invert = TRUE, value = TRUE)) %>%
  group_by(hcsb, method, RR) %>%
  mutate(value = mean(value)) %>% 
  distinct()

cov6 <- p6 %>% 
  filter(method %in% grep("hcsb", grep("cov", p3$method, value = TRUE), invert = TRUE, value = TRUE)) %>%
  group_by(hcsb, method, RR) %>%
  mutate(value = mean(value)) %>% 
  distinct()

cov <- bind_rows(cov3, cov4, cov5, cov6)

myCol <- brewer.pal(9,'Spectral')[c(2:4,8)]

cov <- cov %>% ungroup %>% mutate(method = ifelse(method == "cov.lambda.est", "Test-Positive Only", 
                               ifelse(method == "cov.or.est", "Odds Ratio",
                                      ifelse(method == "cov.tpf.est", "Test-Positive Fraction", "Random Effects"))))

cov %>% filter(method != "Test-Positive Fraction") %>% ggplot(aes(x = as.factor(RR), y = value, group = method, col = hcsb)) + #, shape = hcsb)) + #, shape = method)) + 
  facet_wrap(~method) +
  ylab("Coverage") + 
  geom_hline(yintercept = 0.95, lty = 2, col = 'darkgray') + 
  geom_point(size = 4, aes(x = RR), alpha = 0.8)+
  geom_line(aes(x = RR, y = value, group = hcsb), size = 1.25) + 
  scale_x_reverse() + 
  xlab("True Relative Risk") + 
  ylim(0,1) +
  theme_classic() +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 24, margin = margin(10,0,0,0)), 
        axis.title.y = element_text(size = 22, margin = margin(0,15,0,0))) + 
  scale_color_manual("Health-Care-\nSeeking Behavior", values = c("None" = myCol[4], "Low" = myCol[3], "Medium" = myCol[2], "High" = myCol[1]), 
                     limits = c("High", "Medium", "Low", "None"))
ggsave(here("graphs", "coverage-full-range.png"), device = "png", width = 20, height = 6.5, units = "in")
#ggsave(here("graphs", "coverage-short-range.png"), device = "png", width = 20, height = 6.5, units = "in")



# Power

pow3 <- p3 %>% 
  filter(method %in% grep("hcsb", grep("pow", p3$method, value = TRUE), invert = TRUE, value = TRUE)) %>%
  group_by(hcsb, method, RR) %>%
  mutate(value = mean(value, na.rm = TRUE)) %>% 
  distinct()

pow4 <- p4 %>% 
  filter(method %in% grep("hcsb", grep("pow", p3$method, value = TRUE), invert = TRUE, value = TRUE)) %>%
  group_by(hcsb, method, RR) %>%
  mutate(value = mean(value, na.rm = TRUE)) %>% 
  distinct()

pow5 <- p5 %>% 
  filter(method %in% grep("hcsb", grep("pow", p3$method, value = TRUE), invert = TRUE, value = TRUE)) %>%
  group_by(hcsb, method, RR) %>%
  mutate(value = mean(value, na.rm = TRUE)) %>% 
  distinct()

pow6 <- p6 %>% 
  filter(method %in% grep("hcsb", grep("pow", p3$method, value = TRUE), invert = TRUE, value = TRUE)) %>%
  group_by(hcsb, method, RR) %>%
  mutate(value = mean(value, na.rm = TRUE)) %>% 
  distinct()

power <- bind_rows(pow3, pow4, pow5, pow6)

myCol <- brewer.pal(9,'Spectral')[c(2:4,8)]

power <- power %>% ungroup %>% mutate(method = ifelse(method == "power.lambda.est", "Test-Positive Only", 
                                                  ifelse(method == "power.or.est", "Odds Ratio",
                                                         ifelse(method == "power.tpf.est", "Test-Positive Fraction", "Random Effects"))))

power %>% #filter(method != "Test-Positive Fraction") %>%
  ggplot(aes(x = as.factor(RR), y = value, group = method, col = hcsb)) + #, shape = hcsb)) + #, shape = method)) + 
  facet_wrap(~method) +
  ylab("Power") + 
  geom_hline(yintercept = 0.80, lty = 2, col = 'darkgray') + 
  geom_point(aes(x = RR), size = 4, alpha = 0.8) + 
  geom_line(aes(x = RR, y = value, group = hcsb), size = 1.25) + 
  scale_x_reverse() + 
  xlab("True Relative Risk") + 
  ylim(0,1) +
  theme_classic() +
  #geom_vline(xintercept = c(.55, .45, 0.35), col = 'lightgray', lty = 3) + 
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 24, margin = margin(10,0,0,0)), 
        axis.title.y = element_text(size = 22, margin = margin(0,15,0,0))) + 
  scale_color_manual("Health-Care-\nSeeking Behavior", values = c("None" = myCol[4], "Low" = myCol[3], "Medium" = myCol[2], "High" = myCol[1]), 
                     limits = c("High", "Medium", "Low", "None"))
ggsave(here("graphs", "power-full-range-tpf.png"), device = "png", width = 10, height = 8, units = "in")

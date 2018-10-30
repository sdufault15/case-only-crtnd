# No HCSB
source(here("lib", "performance-functions.R"))

load(here("data", "case-only/all-rr1-hcsb1-09222018.RData"))
load(here("data", "case-only/all-rr6-hcsb1-09222018.RData"))
load(here("data", "case-only/all-rr5-hcsb1-09222018.RData"))
load(here("data", "case-only/all-rr4-hcsb1-09222018.RData"))
load(here("data", "case-only/all-rr3-hcsb1-09222018.RData"))

r1 <- est.fun(rr1, true.lambda = 1, true.hcsb = 1)
r6 <- est.fun(rr6, true.lambda = 0.6, true.hcsb = 1)
r5 <- est.fun(rr5, true.lambda = 0.5, true.hcsb = 1)
r4 <- est.fun(rr4, true.lambda = 0.4, true.hcsb = 1)
r3 <- est.fun(rr3, true.lambda = 0.3, true.hcsb = 1)

total.a <- bind_rows(r1, r6, r5, r4, r3)
total.a$RR <- rep(c(1,0.6,0.5,0.4,0.3), each = 90000)

total.a <- total.a %>% gather("method", "value", 1:9)
all <- total.a %>% arrange(RR)

all %>% filter(method %in% grep("bias", all$method, value = TRUE)) %>%  group_by(method, RR) %>% mutate(value = mean(value)) %>% distinct()

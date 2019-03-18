mixed_effects_function <- function(df){
  # TOO SLOW to run each expansion
  # df_long <- df %>% gather("test.status", "count", cases:OFIs) %>%
  #   mutate(test.status = ifelse(test.status == "cases", 1, 0)) %>%
  #   expandRows("count", count.is.col = TRUE, drop = TRUE) %>%
  #   arrange(Cluster, Treatment)
  
  temp <- df %>%
    group_by(Cluster) %>%
    mutate(ntrials = sum(c(OFIs, cases))) %>%
    ungroup()
  
  gee <- geeglm(I(cases/ntrials) ~ Treatment, data = temp, family = binomial, id = Cluster, weights = ntrials, corstr = "exchangeable", scale.fix = TRUE)
  me <- glmer(I(cases/ntrials) ~ Treatment + (1 | Cluster ), family = binomial, data = temp, weights = ntrials)
  
  ORgee <- exp(summary(gee)$coefficients["Treatment",1]) # Unlogged ORs
  ORme <- exp(summary(me)$coefficients["Treatment", 1])
  
  sdGEE <- summary(gee)$coefficients["Treatment",2]# Logged SDs
  sdME <- summary(me)$coefficients["Treatment", 2]
  
  pvalGEE <- summary(gee)$coefficients["Treatment", 4] # Coefficient p-vals (log scale)
  pvalME <- summary(me)$coefficients["Treatment", 4]
  
  out <- list(OR = ORgee, sd = sdGEE, pval = pvalGEE, OR = ORme, sd = sdME, pval = pvalME)
  
  return(out)
}

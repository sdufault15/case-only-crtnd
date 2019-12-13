mixed_effects_function_counts <- function(df){
  # Taking cluster level counts and fitting poisson models
  gee <- geeglm(cases ~ Treatment, data = df, family = poisson, id = Cluster, corstr = "exchangeable")
  me <- glmer(cases ~ Treatment + (1 | Cluster ), family = poisson, data = df)
  
  ORgee <- exp(summary(gee)$coefficients["Treatment",1]) # Unlogged ORs
  ORme <- exp(summary(me)$coefficients["Treatment", 1])
  
  sdGEE <- summary(gee)$coefficients["Treatment",2]# Logged SDs
  sdME <- summary(me)$coefficients["Treatment", 2]
  
  pvalGEE <- summary(gee)$coefficients["Treatment", 4] # Coefficient p-vals (log scale)
  pvalME <- summary(me)$coefficients["Treatment", 4]
  
  out <- list(OR = ORgee, sd = sdGEE, pval = pvalGEE, OR = ORme, sd = sdME, pval = pvalME)
  
  return(out)
}
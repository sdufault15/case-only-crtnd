aggregate_or_function <- function(df, m, zstar){
  # applies aggregate OR estimation
  # requires generated data (df) with column names Treatment, cases, OFIs
  # m = number of clusters in one arm
  # zstar = Z value associated with desired significance level
  
  
  # collecting all cluster data on those who received the treatment and DID develop dengue
  AplusClusters <- df %>% filter(Treatment == 1) %>% select(cases) %>% unlist() #& PERIOD == "03_05" & iter == 1 & lambda == 0.6 & size == 200
  
  # collecting all cluster data on those who received the treatment and DID NOT develop dengue but OFI
  BplusClusters <- df %>% filter(Treatment == 1) %>% select(OFIs) %>% unlist()
  
  # collecting all cluster data on those who did not receive the treatment and DID develop dengue
  GplusClusters <- df %>% filter(Treatment == 0) %>% select(cases) %>% unlist()
  
  # collecting all cluster data on those who did not receive the treatment and DID NOT develop dengue
  HplusClusters <- df %>% filter(Treatment == 0) %>% select(OFIs) %>% unlist()
  
  Aplus <- sum(AplusClusters) # collecting all individuals who received the treatment and DID develop dengue
  Bplus <- sum(BplusClusters) # collecting all individuals who received the treatment and DID NOT develop dengue but OFI
  Gplus <- sum(GplusClusters) # collecting all individuals who did not receive the treatment and DID develop dengue
  Hplus <- sum(HplusClusters) # collecting all individuals who did not receive the treatment and DID NOT develop dengue but OFI
  
  lambdaHat <- (Aplus*Hplus)/(Gplus*Bplus)
  
  # Adjustment
  A_adj <- AplusClusters*(1/lambdaHat)
  Aplus_adj <- sum(A_adj)
  
  # Variance Calculations
  vD.A <- var(A_adj) 
  vD.G <- var(GplusClusters) 
  vD.B <- var(BplusClusters) 
  vD.H <- var(HplusClusters) 
  vD <- mean(c(vD.A, vD.G)) 
  vDbar <- mean(c(vD.B, vD.H)) 
  
  covAB <- cov(x = A_adj, y = BplusClusters) 
  covGH <- cov(x = GplusClusters, y = HplusClusters) 
  
  covABplus <- m*covAB*(m/(2*m)) # according to previous equation
  covGHplus <- m*covGH*(m/(2*m)) # according to previous equation
  covFINAL <- mean(c(covABplus, covGHplus))
  
  nD <- sum(c(Aplus_adj,Gplus)) # number of cases 
  nDbar <- sum(c(Bplus, Hplus)) # number of controls 
  
  varlogOR <- (16/(nD^2))*(m/2)*vD + (16/(nDbar^2))*(m/2)*vDbar - 2*( (nD*nDbar) / (Aplus_adj*(nD - Aplus_adj)*Bplus*(nDbar - Bplus)))*covFINAL
  
  sd <- sqrt(varlogOR) # for comparison with logistic regression output
  
  # Significance
  OR_est <- lambdaHat
  sig <- 1 - between(0,
                 log(OR_est) - zstar*sd,
                 log(OR_est) + zstar*sd)
  
  out <- c(OR_est, sd, sig)
  return(out)
}
test_positive_function <- function(df, m, zstar){
  # applies aggregate OR estimation
  # requires generated data (df) with column names Treatment, cases, OFIs
  # m = number of clusters in one arm
  # zstar = Z value associated with desired significance level
  
  
  # collecting all cluster data on those who received the treatment and DID develop dengue
  AplusClusters <- df %>% filter(Treatment == 1) %>% select(cases) %>% unlist() #& PERIOD == "03_05" & iter == 1 & lambda == 0.6 & size == 200
  
  # collecting all cluster data on those who did not receive the treatment and DID develop dengue
  GplusClusters <- df %>% filter(Treatment == 0) %>% select(cases) %>% unlist()
  
  Aplus <- sum(AplusClusters) # collecting all individuals who received the treatment and DID develop dengue
  Gplus <- sum(GplusClusters) # collecting all individuals who did not receive the treatment and DID develop dengue
  
  lambdaHat <- Aplus/Gplus
  
  # Adjustment
  A_adj <- AplusClusters*(1/lambdaHat)
  Aplus_adj <- sum(A_adj)
  
  # Variance Calculations
  vD.A <- var(A_adj) 
  vD.G <- var(GplusClusters) 
  vD <- mean(c(vD.A, vD.G))  # pooled variance
  
  nD <- sum(c(Aplus_adj,Gplus)) # number of cases 
  
  varlogOR <- (16/(nD^2))*(m/2)*vD 
  
  sd <- sqrt(varlogOR) # for comparison with logistic regression output
  
  # Significance T Test
  Tstat <- (Aplus - Gplus)/(sqrt(2*m*vD))
  p.val <- 2*pt(abs(Tstat), df = 2*(m - 1), lower.tail = FALSE)
  
  out <- c(lambdaHat, sd, p.val)
  return(out)
}
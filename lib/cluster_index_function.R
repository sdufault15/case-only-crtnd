cluster.index.fun <- function(dataframe, rrIN, period, n.cases, ratio, alpha1, alpha2, beta1, beta2){
  # This function modifies the dataset according to a deterministic implementation of the
  # exposure index and the intervention RR. 
  # Then the random effects OR is evaluated.
  meORMatrix <- vector('list', length = length(period))
  sdMEMatrix <- vector('list', length = length(period))
  pvalmeORMatrix <- vector('list', length = length(period))
  
  iter1 <- 1
  
  for (j in period){
    dta <- dataframe[dataframe$Period == j,] # subset data to just the time period of interest
    txDtaF <- txtSet(dta) # subset out the treatment allocations
    m <- nrow(dta)/2 # number of clusters in each arm
    
    ORme <- NULL
    pvalME <- NULL
    sdME <- NULL
    
    for (i in 1:ncol(txDtaF)){
      txDta <- txDtaF[,i] # select a treatment allocation
      set.seed(12345)
      
      index.vec <- ifelse(txDta == 1, rbeta(m, alpha2,beta2), rbeta(m, alpha1,beta1))
      
      propCases <- dta$Cases/sum(dta$Cases) # proportion of cases per cluster
      propOFI <- dta$OFI/sum(dta$OFI) # proportion of OFI per cluster
      nControls <- ratio*n.cases*propOFI # assigning controls to clusters
      nStarProp <- propCases*(rrIN*index.vec + (1-index.vec)) # applying the intervention according to index
      nStarProp <- nStarProp/sum(nStarProp) # Re-standardizing proportions so they sum to 1
      nCases <- nStarProp*n.cases # assigning cases to clusters
      
      # NEED TO RESHAPE THE DATA
      freq.1 <- nCases
      freq.0 <- nControls
      tempWide <- data.frame(dta$clust, txDta, freq.1, freq.0)
      tempWide <- reshape(data = tempWide, direction = "long", varying = 3:4, sep = "." )

      v <- round(as.matrix(tempWide$freq))
      #mod1 <- glm(time ~ tempWide[[2]] , family = "binomial", data = tempWide, weights = v) 
      #g1 <- geeglm(time ~ tempWide[[2]], data = tempWide, family = binomial, weights = v, id = tempWide[[1]], corstr = "exchangeable", scale.fix = TRUE)
      me1 <- glmer(time ~ tempWide[[2]] + (1 | id ), family = binomial, data = tempWide, weights = v)
      
      ORme <- rbind(ORme, exp(summary(me1)$coefficients[2]))
      sdME <- rbind(sdME, summary(me1)$coefficients[4])
      pvalME <- rbind(pvalME, summary(me1)$coefficients[2,4])
    }
    meORMatrix[[iter1]] <- ORme
    sdMEMatrix[[iter1]] <- sdME
    pvalmeORMatrix[[iter1]] <- pvalME
    
    iter1 <- iter1 + 1
  }
  
  output <- list(OR_me = meORMatrix, sd_me = sdMEMatrix, pval_me = pvalmeORMatrix)
  return(output)
}
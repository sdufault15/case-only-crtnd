lambda_gamma_function <- function(dta, rrIN, period, ncases = 1000, hcsb = FALSE){
  # This function uses only the cases to get lambda when hcsb = FALSE
  # It uses only the controls to get gamma when hcsb = TRUE
  # make sure rrIN = 1 when attempting to get gamma
  tptn = 1 - as.numeric(hcsb) # whether this is the test positives or test negatives
  
  out1 <- out2 <- out3 <- out4 <- out5 <- out6 <-  NULL
  sigMatrix <-NULL
  TOTpvals <- vector("list", length(rrIN)) # p-values for the T-tests
  TOTtVals <- vector("list", length(rrIN)) # T test statistics
  params <- vector("list", length(rrIN)) # storage of estimated params
  var.log.params <- vector('list', length(rrIN)) # storage of standard deviations
  varTvals <- vector('list', length(rrIN)) # not T statistics, but difference in arm specific means
  coverage <- vector('list', length(rrIN)) # coverage of log-lambda estimates
  
  iter1 <- 1 # counts number of iterations (if there are 4 different relative risks, iter1 should end at 5)
  
  for (j in period){
    temp <- dta[dta$Period == j,] # subset data to just the time period of interest
    rr <- rrIN
    n1 <- ncases
    
    prop <- tptn*(temp$Cases/sum(temp$Cases)) + (1-tptn)*(temp$OFI/sum(temp$OFI))
    
    # Separate treatment allocations from other data
    txDta <- txtSet(temp)
    
    out1 <- out2 <- out3 <- out4 <- out5 <- out6 <- NULL
    
    for (i in 1:ncol(txDta)){
      tx.temp <- txDta[[i]] # Select the Treatment Allocation
      nStarProp <- prop # Set up for adjustment of proportion of cases according to treatment allocation
      nStarProp[tx.temp == tptn] <- nStarProp[tx.temp == tptn]*rr # Modify those clusters that are within the treatment arm
      nStarProp <- nStarProp/sum(nStarProp) # Make sure the proportions sum to 1
      nCases <- nStarProp*n1 # Assign cases to clusters according to proportion of cases per cluster
      m <- length(nStarProp[tx.temp == tptn])
      
      Tval <- sum(nCases[tx.temp == 1]) - sum(nCases[tx.temp == 0]) # T  
      vD <- mean(c(var(nCases[tx.temp == 1]),var(nCases[tx.temp == 0]))) # pooled variance V_D
      varTval <- 2*m*vD # pooled variance estimate
      pval <- dt(Tval/sqrt(varTval), df = 2*(m-1))
      
      param.hat <- sum(nCases[tx.temp == 1])/sum(nCases[tx.temp == 0])
      var.log.param <- (16/(sum(nCases)^2))*(m/2)*vD
      
      coverage.v <- as.numeric((between(log(rrIN), log(param.hat) - 1.96*sqrt(var.log.param), log(param.hat) + 1.96*sqrt(var.log.param))))
      
      out1 <- rbind(out1, Tval)
      out2 <- rbind(out2, pval)
      out3 <- rbind(out3, param.hat)
      out4 <- rbind(out4, var.log.param)
      out5 <- rbind(out5, varTval)
      out6 <- rbind(out6, coverage.v)
    }
    
    TOTtVals[[iter1]] <- out1
    TOTpvals[[iter1]] <- out2
    params[[iter1]] <- out3
    var.log.params[[iter1]] <- out4
    varTvals[[iter1]] <- out5
    coverage[[iter1]] <- out6
    
    iter1 <- iter1 + 1
  }
  output <- list(tVals = TOTtVals, pVals = TOTpvals, param.est = params, 
                 var.log.param = var.log.params, varTvals = varTvals, coverage = coverage)
  
  return(output)
}

gamma_function <- function(dta, period, ncases = 1000, gamma){
  # It uses only the controls to get gamma where gamma is B/H 
  library(dplyr)
  
  out1 <- out2 <- out3 <- out4 <- out5 <- out6 <-  NULL
  sigMatrix <-NULL
  TOTpvals <- vector("list", length(gamma)) # p-values for the T-tests
  TOTtVals <- vector("list", length(gamma)) # T test statistics
  params <- vector("list", length(gamma)) # storage of estimated params
  var.log.params <- vector('list', length(gamma)) # storage of standard deviations
  varTvals <- vector('list', length(gamma)) # not T statistics, but difference in arm specific means
  coverage <- vector('list', length(gamma)) # coverage of log-lambda estimates
  
  iter1 <- 1 # counts number of iterations (if there are 4 different relative risks, iter1 should end at 5)
  
  for (j in period){
    temp <- dta[dta$Period == j,] # subset data to just the time period of interest
    n1 <- ncases
    
    # Separate treatment allocations from other data
    txDta <- txtSet(temp)
    
    out1 <- out2 <- out3 <- out4 <- out5 <- out6 <- NULL
    
    for (i in 1:ncol(txDta)){
      tx.temp <- txDta[[i]] # Select the Treatment Allocation
      temp$OFI[tx.temp == 1] <- temp$OFI[tx.temp == 1]*gamma
      prop <- (temp$OFI/sum(temp$OFI))
      
      nOFI <- prop*n1 # Assign cases to clusters according to proportion of cases per cluster
      m <- length(prop[tx.temp == 1])
      
      Tval <- sum(nOFI[tx.temp == 1]) - sum(nOFI[tx.temp == 0]) # T  
      vD <- mean(c(var(nOFI[tx.temp == 1]),var(nOFI[tx.temp == 0]))) # pooled variance V_D
      varTval <- 2*m*vD # pooled variance estimate
      pval <- dt(Tval/sqrt(varTval), df = 2*(m-1))
      
      param.hat <- sum(nOFI[tx.temp == 1])/sum(nOFI[tx.temp == 0])
      var.log.param <- (16/(sum(nOFI)^2))*(m/2)*vD
      
      coverage.v <- as.numeric((between(log(gamma), log(param.hat) - 1.96*sqrt(var.log.param), log(param.hat) + 1.96*sqrt(var.log.param))))
      
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

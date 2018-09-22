case_only_function <- function(data, period, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 1){
  # This function can apply differential health care seeking behavior (lambda.hcsb) as well as an intervention
  # RR. 
  # It then estimates from the data and permuted treatment allocations estimates of the intervention RR and
  # the health care seeking behavior RR.
  # Futher, it applies the Odds Ratio method proposed in the previous paper for a comparison of performance 
  # under the setting of differential health care seeking behavior
  
  # data = data frame with columns labelled "Period", "OFI", "Cases", "tx1",... "txp" for p possible allocations
  # period = the periods of the data over which to perform estimation
  # n.obs.pos = the number of test positives desired
  # n.obs.neg = the number of test negatives desired
  # lambda.int = the true intervention relative risk
  # lambda. hcsb = the true health care seeking behavior relative risk
  
  lambda.int.list <- lambda.hcsb.list <- lambda.OR.list <- vector('list', length(period))
  v.log.lambda.int.list <- v.log.lambda.hcsb.list <- v.log.lambda.OR.list <- vector('list', length(period))
  cov.int.list <- cov.hcsb.list <- cov.OR.list <- vector('list', length(period))
  p.int.list <- p.hcsb.list <- signif.OR.list <- vector('list', length(period))
  T.int.list <- T.hcsb.list <- vector('list', length(period))
  
  out1 <- out2 <- out3 <- out4 <- out5 <- out6 <- out7 <- out8 <- out9 <- out10 <- out11 <- out12 <- out13 <- out14 <- NULL
  iter1 <- 1
  
  for (j in period){
    
    out1 <- out2 <- out3 <- out4 <- out5 <- out6 <- out7 <- out8 <- out9 <- out10 <- out11 <- out12 <- out13 <- out14 <- NULL
    
    current <- data[data$Period == j,] # identifying the distinct period of interest
    txDta <- txtSet(current)
    m <- nrow(current)/2
    
    
    for (i in 1:ncol(txDta)){
      temp <- current # resetting the working data
      tx.temp <- txDta[[i]] # selecting a specific treatment allocation
      
      ##### Apply health care seeking behavior
      temp$OFI[tx.temp == 1] <- temp$OFI[tx.temp == 1]*lambda.hcsb
      temp$Cases[tx.temp == 1] <- temp$Cases[tx.temp == 1]*lambda.hcsb
      
      # Estimate lambda.hcsb
      prop.OFI <- temp$OFI/sum(temp$OFI)
      n.ofi <- prop.OFI*n.obs.neg
      B <- sum(n.ofi[tx.temp == 1])
      H <- sum(n.ofi[tx.temp == 0])
      
      T.hcsb <- B - H # test statistic
      vD.hcsb <- mean(c(var(n.ofi[tx.temp == 1]),var(n.ofi[tx.temp == 0]))) # pooled variance V_D
      var.T.hcsb <- 2*m*vD.hcsb # pooled variance estimate
      pval.hcsb <- dt(T.hcsb/sqrt(var.T.hcsb), df = 2*(m-1))
      
      lambda.hcsb.hat <- B/H
      var.log.lambda.hcsb <- (16/(sum(n.ofi)^2))*(m/2)*vD.hcsb
      coverage.hcsb <- as.numeric((between(log(lambda.hcsb), 
                                        log(lambda.hcsb.hat) - 1.96*sqrt(var.log.lambda.hcsb), 
                                        log(lambda.hcsb.hat) + 1.96*sqrt(var.log.lambda.hcsb))))
      
      
      ##### Apply intervention effect
      prop.Cases <- temp$Cases/sum(temp$Cases)
      prop.Cases[tx.temp == 1] <- prop.Cases[tx.temp == 1]*lambda.int
      
      # Estimate lambda.int
      prop.Cases <- prop.Cases/sum(prop.Cases)
      n.cases <- prop.Cases*n.obs.pos
      A <- sum(n.cases[tx.temp == 1])
      G <- sum(n.cases[tx.temp == 0])
      
      T.int <- A - G # test statistic
      vD.int <- mean(c(var(n.cases[tx.temp == 1]),var(n.cases[tx.temp == 0]))) # pooled variance V_D
      var.T.int <- 2*m*vD.int # pooled variance estimate
      pval.int <- dt(T.int/sqrt(var.T.int), df = 2*(m-1))
      
      lambda.int.hat <- A/G
      var.log.lambda.int <- (16/(sum(n.cases)^2))*(m/2)*vD.int
      coverage.int <- as.numeric((between(log(lambda.int), 
                                        log(lambda.int.hat) - 1.96*sqrt(var.log.lambda.int), 
                                        log(lambda.int.hat) + 1.96*sqrt(var.log.lambda.int))))
      
      # Estimate Odds Ratio Method
      lambda.OR.hat <- (A*H)/(B*G)
      A.cases.adj <- n.cases[tx.temp == 1]*lambda.OR.hat # adjustment
      A_adj <- sum(A.cases.adj)
      
      vD.A <- var(A.cases.adj) 
      vD.G <- var(n.cases[tx.temp == 0]) 
      vD.B <- var(n.ofi[tx.temp == 1]) 
      vD.H <- var(n.ofi[tx.temp == 0]) 
      vD <- mean(c(vD.A, vD.G)) 
      vDbar <- mean(c(vD.B, vD.H)) 
      
      covAB <- cov(x = A.cases.adj, y = n.ofi[tx.temp == 1]) # empirical estimate of cov(Aj, Bj)
      covGH <- cov(x = n.cases[tx.temp == 0], y = n.ofi[tx.temp == 0]) # empirical estimate of cov(Gj, Hj)
      
      covABplus <- m*covAB*(m/(2*m)) # according to previous equation
      covGHplus <- m*covGH*(m/(2*m)) # according to previous equation
      covFINAL <- mean(c(covABplus, covGHplus))
      
      var.log.lambda.OR <- (16/(sum(n.cases)^2))*(m/2)*vD + (16/(sum(n.ofi)^2))*(m/2)*vDbar - 2*( (sum(n.cases)*sum(n.ofi)) / (A_adj*(sum(n.cases) - A_adj)*sum(n.ofi[tx.temp == 1])*(sum(n.ofi) - sum(n.ofi[tx.temp == 1]))))*covFINAL
      
      coverage.or <- as.numeric((between(log(lambda.int), 
                                          log(lambda.OR.hat) - 1.96*sqrt(var.log.lambda.OR), 
                                          log(lambda.OR.hat) + 1.96*sqrt(var.log.lambda.OR))))
      
      print(var.log.lambda.OR)
      sig.or <- 1 - as.numeric((between(0, 
                                    log(lambda.OR.hat) - 1.96*sqrt(var.log.lambda.OR), 
                                    log(lambda.OR.hat) + 1.96*sqrt(var.log.lambda.OR))))
      
      # Storage for each treatment allocation within one period
      out1 <- c(out1, lambda.int.hat)
      out2 <- c(out2, lambda.hcsb.hat)
      out3 <- c(out3, lambda.OR.hat)
      
      out4 <- c(out4, var.log.lambda.int)
      out5 <- c(out5, var.log.lambda.hcsb)
      out6 <- c(out6, var.log.lambda.OR)
      
      out7 <- c(out7, coverage.int)
      out8 <- c(out8, coverage.hcsb)
      out9 <- c(out9, coverage.hcsb)
      
      out10 <- c(out10, pval.int)
      out11 <- c(out11, pval.hcsb)
      out12 <- c(out12, sig.or)
      
      out13 <- c(out13, T.int)
      out14 <- c(out14, T.hcsb)
    }
    
    # Storage of the results in each period
    lambda.int.list[[iter1]] <- out1
    lambda.hcsb.list[[iter1]] <- out2
    lambda.OR.list[[iter1]] <- out3
    
    v.log.lambda.int.list[[iter1]] <- out4
    v.log.lambda.hcsb.list[[iter1]] <- out5
    v.log.lambda.OR.list[[iter1]] <- out6
    
    T.int.list[[iter1]] <- out13
    T.hcsb.list[[iter1]] <- out14
    
    cov.int.list[[iter1]] <- out7
    cov.hcsb.list[[iter1]] <- out8
    cov.OR.list[[iter1]] <- out9
    
    p.int.list[[iter1]] <- out10
    p.hcsb.list[[iter1]] <- out11
    signif.OR.list[[iter1]] <- out12
    
    iter1 <- iter1 + 1
  }
  
  # Organizing Output
  
  intervention.estimation <- list(lambda.int.est = lambda.int.list, var.log.lambda.int.est = v.log.lambda.int.list, 
                                  coverage.log.lambda.int = cov.int.list, pvals.log.lambda.int = p.int.list, T.stats.int = T.int.list)
  hcsb.estimation <- list(lambda.hcsb.est = lambda.hcsb.list, var.log.lambda.hcsb.est = v.log.lambda.hcsb.list, 
                                  coverage.log.lambda.hcsb = cov.hcsb.list, pvals.log.lambda.hcsb = p.hcsb.list, T.stats.hcsb = T.hcsb.list)
  OR.estimation <- list(lambda.OR.est = lambda.OR.list, var.log.lambda.OR.est = v.log.lambda.OR.list, 
                        coverage.log.lambda.OR = cov.OR.list, signficance.log.lambda.OR = signif.OR.list)
  
  out <- list(intervention = intervention.estimation, hcsb = hcsb.estimation, OR = OR.estimation)
  return(out)
}


list.hcsb <- list()

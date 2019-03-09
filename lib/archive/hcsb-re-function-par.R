hcsb_re_function_par <- function(data, period, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 1){
  # This function can apply differential health care seeking behavior (lambda.hcsb) as well as an intervention
  # RR. 
  # It then estimates from the data and permuted treatment allocations estimates of the intervention RR and
  # the health care seeking behavior RR by the random effects methods described in the 
  # previous paper.
  
  # data = data frame with columns labelled "Period", "OFI", "Cases", "tx1",... "txp" for p possible allocations
  # period = the periods of the data over which to perform estimation
  # n.obs.pos = the number of test positives desired
  # n.obs.neg = the number of test negatives desired
  # lambda.int = the true intervention relative risk
  # lambda. hcsb = the true health care seeking behavior relative risk
  library(splitstackshape)
  library(dplyr)
  library(tidyr)
  library(lme4)
  
  #lambda.tpf.list <- lambda.re.list <- vector('list', length(period))
  #v.log.lambda.tpf.list <- v.log.lambda.re.list <- vector('list', length(period))
  #cov.tpf.list <- cov.re.list <- vector('list', length(period))
  #p.tpf.list <- p.re.list <- vector('list', length(period))
  #T.int.list <- vector('list', length(period))
  results <- vector('list', length(period))
  
  #out1 <- out2 <- out3 <- out4 <- out5 <- out6 <- out7 <- out8 <- out9 <- NULL
  iter1 <- 1
  
  for (j in period){
    
    #out1 <- out2 <- out3 <- out4 <- out5 <- out6 <- out7 <- out8 <- out9 <- out10 <- out11 <- out12 <- out13 <- out14 <- NULL
    
    current <- data[data$Period == j,] # identifying the distinct period of interest
    txDta <- txtSet(current)
    m <- nrow(current)/2
    
    out <- NULL
    for (i in 1:ncol(txDta)){
      temp <- current # resetting the working data
      tx.temp <- txDta[[i]] # selecting a specific treatment allocation
      
      ##### Apply health care seeking behavior
      temp$OFI[tx.temp == 1] <- temp$OFI[tx.temp == 1]*lambda.hcsb
      temp$Cases[tx.temp == 1] <- temp$Cases[tx.temp == 1]*lambda.hcsb
      
      prop.OFI <- temp$OFI/sum(temp$OFI)
      
      ##### Apply intervention effect
      prop.Cases <- temp$Cases/sum(temp$Cases)
      prop.Cases[tx.temp == 1] <- prop.Cases[tx.temp == 1]*lambda.int
      n.cases <- prop.Cases*n.obs.pos
      n.controls <- prop.OFI*n.obs.neg
      
      #### Random Effects Approach:
      # NEED TO RESHAPE THE DATA
      freq.1 <- n.cases
      freq.0 <- n.controls
      tempWide <- data.frame(id = current$clust, tx.temp, freq.1, freq.0)
      tempLong <- tempWide %>% gather("teststatus", "count", 3:4) %>% mutate(teststatus = ifelse(teststatus == "freq.1", 1, 0),
                                                                             v = round(count)) %>% select(-count)
      tempLong <- expandRows(tempLong, "v", count.is.col = TRUE, drop = TRUE)
  
      me1 <- glmer(teststatus ~ tx.temp + (1 | id ), family = binomial, data = tempLong) 
      re.int.hat <- exp(summary(me1)$coefficients[2])
      var.log.re.int <- (summary(me1)$coefficients[4])^2
      coverage.log.re.int <- between(log(lambda.int),
                                     log(re.int.hat) - 1.96*summary(me1)$coefficients[4],
                                     log(re.int.hat) + 1.96*summary(me1)$coefficients[4])
      pvals.log.re.int <- summary(me1)$coefficients[2,4]

      
      # Storage for each treatment allocation within one period
      out <- bind_rows(out, data.frame(re.int.hat, var.log.re.int, coverage.log.re.int, pvals.log.re.int))

    }
    
    # Storage of the results in each period
    results[[iter1]] <- out
    
    iter1 <- iter1 + 1
  }
  return(results)
}

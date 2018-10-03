hcsb_tpf_re_function <- function(data, period, n.obs.pos = 1000, n.obs.neg = 1000, lambda.int = 1, lambda.hcsb = 1){
  # This function can apply differential health care seeking behavior (lambda.hcsb) as well as an intervention
  # RR. 
  # It then estimates from the data and permuted treatment allocations estimates of the intervention RR and
  # the health care seeking behavior RR by the test-positive fraction and random effects methods described in the 
  # previous paper.
  
  # data = data frame with columns labelled "Period", "OFI", "Cases", "tx1",... "txp" for p possible allocations
  # period = the periods of the data over which to perform estimation
  # n.obs.pos = the number of test positives desired
  # n.obs.neg = the number of test negatives desired
  # lambda.int = the true intervention relative risk
  # lambda. hcsb = the true health care seeking behavior relative risk
  library(splitstackshape)
  
  lambda.tpf.list <- lambda.re.list <- vector('list', length(period))
  v.log.lambda.tpf.list <- v.log.lambda.re.list <- vector('list', length(period))
  cov.tpf.list <- cov.re.list <- vector('list', length(period))
  p.tpf.list <- p.re.list <- vector('list', length(period))
  T.int.list <- vector('list', length(period))
  
  out1 <- out2 <- out3 <- out4 <- out5 <- out6 <- out7 <- out8 <- out9 <- NULL
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
      
      prop.OFI <- temp$OFI/sum(temp$OFI)
      
      
      ##### Apply intervention effect
      prop.Cases <- temp$Cases/sum(temp$Cases)
      prop.Cases[tx.temp == 1] <- prop.Cases[tx.temp == 1]*lambda.int
      n.cases <- prop.Cases*n.obs.pos
      n.controls <- prop.OFI*n.obs.neg
      
      ratio <- n.obs.neg/n.obs.pos
      
      #### Test-Positive Fraction Approach:
      prop <- n.cases/(n.cases + n.controls) # a_j  
      test <- t.test(prop ~ as.factor(tx.temp), var.equal = TRUE) # Pooled variance
      ET <- diff(test$estimate)
      tpf.int.hat <- quad(ET, ratio)[which(quad(ET, ratio) > 0)]
      pvals.tpf.int <- test$p.value
      T.stats.tpf.int <- ET
      
      # To get coverage
      coverage.tpf.int <- between(lambda.int,
                     quad(-test$conf.int[2], ratio)[which(quad(-test$conf.int[2], ratio) > 0)],
                     quad(-test$conf.int[1], ratio)[which(quad(-test$conf.int[1], ratio) > 0)])
      
      # To get the estimated variances, take the average of the variance of the logged proportions in each arm
      var.tpf.int <- mean(c(var(prop[tx.temp == 1]), var(prop[tx.temp == 0])))
      
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
      coverage.log.re.int <- between(lambda.int,
                                     log(re.int.hat) - 1.96*summary(me1)$coefficients[4],
                                     log(re.int.hat) + 1.96*summary(me1)$coefficients[4])
      pvals.log.re.int <- summary(me1)$coefficients[2,4]
      
      # ORgee <- rbind(ORgee, exp(summary(g1)$coefficients[2,1])) # Unlogged ORs
      # ORme <- rbind(ORme, exp(summary(me1)$coefficients[2]))
      # ORstand <- rbind(ORstand, exp(summary(mod1)$coefficients[2]))
      # sdGEE <- rbind(sdGEE, summary(g1)$coefficients[2,2]) # Logged SDs
      # sdME <- rbind(sdME, summary(me1)$coefficients[4])
      # sdStand <- rbind(sdStand, summary(mod1)$coefficients[4])
      # pvalGEE <- rbind(pvalGEE, summary(g1)$coefficients[2,4])
      # pvalME <- rbind(pvalME, summary(me1)$coefficients[2,4])
      # pvalStand <- rbind(pvalStand, summary(mod1)$coefficients[2,4])
      
      # Storage for each treatment allocation within one period
      out1 <- c(out1, tpf.int.hat)
      out2 <- c(out2, re.int.hat)
      
      out3 <- c(out3, var.tpf.int)
      out4 <- c(out4, var.log.re.int)
      
      out5 <- c(out5, coverage.tpf.int)
      out6 <- c(out6, coverage.log.re.int)
      
      out7 <- c(out7, pvals.tpf.int)
      out8 <- c(out8, pvals.log.re.int)
      
      out9 <- c(out9, T.stats.tpf.int)

    }
    
    # Storage of the results in each period
    lambda.tpf.list[[iter1]] <- out1
    lambda.re.list[[iter1]] <- out2
    
    v.log.lambda.tpf.list[[iter1]] <- out3
    v.log.lambda.re.list[[iter1]] <- out4
    
    T.int.list[[iter1]] <- out9
    
    cov.tpf.list[[iter1]] <- out5
    cov.re.list[[iter1]] <- out6
    
    p.tpf.list[[iter1]] <- out7
    p.re.list[[iter1]] <- out8
    
    iter1 <- iter1 + 1
  }
  
  # Organizing Output
  
  tpf.estimation <- list(lambda.tpf.int.est = lambda.tpf.list, var.lambda.tpf.int.est = v.log.lambda.tpf.list, 
                                  coverage.lambda.tpf.int = cov.tpf.list, pvals.lambda.tpf.int = p.tpf.list, T.stats.tpf.int = T.int.list)
  re.estimation <- list(lambda.re.int.est = lambda.re.list, var.log.lambda.re.int.est = v.log.lambda.re.list, 
                         coverage.log.lambda.re.int = cov.re.list, pvals.log.lambda.re.int = p.re.list)
  
  out <- list(test.positive.fraction = tpf.estimation, random.effects = re.estimation)
  return(out)
}
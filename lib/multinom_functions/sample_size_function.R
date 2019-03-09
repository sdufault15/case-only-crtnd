sample_size_function <- function(df, periods, lambdas = c(1,0.6,0.5, 0.4,0.3), ns = c(200, 400, 600, 800, 1000), print = FALSE){
  # df is a dataframe with treatment allocations labeled 'tx"
  # periods is a vector of historic periods
  # lambdas is a vector of intervention effects
  # ns is a vector of sample sizes
  
  treatment.status <- unlist(grep("tx", names(df), value = TRUE))
  
  #print(treatment.status)
  iter <- 1
  df_gen <- NULL
  df_temp <- NULL
  df_temp_2 <- NULL
  
  for (PERIOD in periods){
    df_temp <- df %>% filter(Period == PERIOD)
    #print(df_temp)
    
    for (TX in treatment.status){
      print(c("Iteration: ", iter, "Period: ", PERIOD, "Tx: ", TX))
      
      df_temp_2 <- df_temp %>% select(clust:Cases, TX) %>%
        mutate(propCases = Cases/sum(Cases),
               propOFI = OFI/sum(OFI))
      #print(names(df_temp_2))
      #print(summary(df_temp_2))
      #propCases <- df_temp_2$Cases/sum(df_temp_2$Cases)
      #print(propCases)
      #propOFI <- df_temp_2$OFI/sum(df_temp_2$OFI)
      #print(propOFI)
      
      df_gen <- rbind(df_gen, 
                      cbind(iter, PERIOD, 
                            generate_function(cluster = df_temp_2$clust, lambda = lambdas, n = ns, 
                                              tx = unlist(df_temp_2[,5]), vecP.observed = unlist(df_temp_2$propCases), 
                                              vecP2.observed = unlist(df_temp_2$propOFI), print = print)))
      iter <- iter + 1
      
    }
  }
  return(df_gen)
}
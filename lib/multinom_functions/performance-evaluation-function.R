performance_function <- function(df, m, zstar, estimator = c("AggOR", "ME.GEE", "TP", "ME.GEE.COUNT")){
  
  df_temp <- as_tibble(df) %>% nest_legacy(-PERIOD, -iter, -lambda, -hcsb, -size)
  
  # Case Count-Based Models Only
  if("ME.GEE.COUNT" %in% estimator){
    df_temp <- df_temp %>% mutate(ME.GEE.IRR = future_map(data, ~mixed_effects_function_counts(.))) %>%
      mutate(IRR.gee = map_dbl(ME.GEE.IRR, 1),
             IRR.sd.gee = map_dbl(ME.GEE.IRR, 2),
             IRR.pval.gee = map_dbl(ME.GEE.IRR, 3),
             IRR.me = map_dbl(ME.GEE.IRR, 4),
             IRR.sd.me = map_dbl(ME.GEE.IRR, 5),
             IRR.pval.me = map_dbl(ME.GEE.IRR, 6))
  }
  
  if("TP" %in% estimator){
    df_temp <- df_temp %>% mutate(TP = future_map(data, ~test_positive_function(., m = m, zstar = zstar))) %>%
      mutate(OR.tp = map_dbl(TP, 1),
             sd.tp = map_dbl(TP, 2),
             pval.tp = map_dbl(TP, 3))
  } 
  
  # Case and "Control" Models
  if("AggOR" %in% estimator){
    df_temp <- df_temp %>% mutate(AggOR = future_map(data, ~aggregate_or_function(., m, zstar))) %>% 
      mutate(OR.agg = map_dbl(AggOR, 1),
             sd.agg = map_dbl(AggOR, 2),
             sig.agg = map_dbl(AggOR, 3))
  }
  
  if("ME.GEE" %in% estimator){
    df_temp <- df_temp %>% mutate(ME.GEE = future_map(data, ~mixed_effects_function(.))) %>%
      mutate(OR.gee = map_dbl(ME.GEE, 1),
             sd.gee = map_dbl(ME.GEE, 2),
             pval.gee = map_dbl(ME.GEE, 3),
             OR.me = map_dbl(ME.GEE, 4),
             sd.me = map_dbl(ME.GEE, 5),
             pval.me = map_dbl(ME.GEE, 6))
  }
  
  
  return(df_temp)
}

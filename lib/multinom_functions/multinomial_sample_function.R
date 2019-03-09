multinom_sample_function <- function(lambda, vecP.observed, tx.status, n, print = FALSE){
  # lambda is a numeric
  # vecP.observed is a vector of observed proportions
  # tx.status is a vector of 1s and 0s denoting intervention status
  # n is the size of the sample
  # print is useful for checking errors
  
  # Modifies and draws a multinomial sample for an intervened upon distribution of counts
  if (print == TRUE){ 
    print(tx.status)
    print(vecP.observed)
  }
  vecP.intervention <- vecP.observed*tx.status*lambda + vecP.observed*(1-tx.status)
  if (print == TRUE){
    print(vecP.intervention)
  }
  vecP.intervention <- vecP.intervention/sum(vecP.intervention)
  if (print == TRUE){
    print(vecP.intervention)
  }
  counts <- rmultinom(1, size = n, prob = vecP.intervention)
  out <- data.frame(counts)
  return(out)
}
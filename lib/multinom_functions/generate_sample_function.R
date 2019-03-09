generate_function <- function(cluster, lambda, n, tx, case_proportions, control_proportions, print = FALSE){
  # lambda is a vector of lambdas
  # n is a vector of ns
  # cluster is a vector of cluster ids
  # tx is a vector of 1s and 0s denoting intervention assignment
  # case_proportions is the vector of observed case proportions
  # control_proportions is the vector of observed control proportions
  # print is useful for errors
  out <- NULL
  
  for (size in n){
    for (lamb in lambda){
      out <- rbind(out, cbind(cluster, tx, lamb, size, 
                              cases = multinom_sample_function(lambda = lamb, vecP.observed = case_proportions, tx.status = tx, n = size), 
                              OFIs = multinom_sample_function(lambda = 1, vecP.observed = control_proportions, tx.status = tx, n = 4*size)))
    }
  }
  out <- data.frame(out, row.names = NULL)
  names(out) <- c("Cluster", "Treatment", "lambda", "size", "cases", "OFIs")
  return(out)
}
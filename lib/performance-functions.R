# Performance Functions

est.fun <- function(dta, true.lambda, true.hcsb) {
  bias.lambda.est <- unlist(dta$intervention$lambda.int.est) - true.lambda
  bias.hcsb.est <- unlist(dta$hcsb$lambda.hcsb.est) - true.hcsb
  bias.or.est <- unlist(dta$OR$lambda.OR.est) - true.lambda
  
  cov.lambda.est <- unlist(dta$intervention$coverage.log.lambda.int)
  cov.hcsb.est <- unlist(dta$hcsb$coverage.log.lambda.hcsb)
  cov.or.est <- unlist(dta$OR$coverage.log.lambda.OR)
  
  power.lambda.est <- unlist(dta$intervention$pvals.log.lambda.int) < 0.05
  power.hcsb.est <- unlist(dta$hcsb$pvals.log.lambda.hcsb) < 0.05
  power.or.est <- unlist(dta$OR$signficance.log.lambda.OR)
  
  out <- data.frame(bias.lambda.est, bias.hcsb.est, bias.or.est, 
                    cov.lambda.est, cov.hcsb.est, cov.or.est,
                    power.lambda.est, power.hcsb.est, power.or.est)
  return(out)
}


est.fun.comp <- function(dta.in.re, dta.in.tpf, true.lambda) {
  
  bias.tpf.est <- unlist(lapply(dta.in.tpf, function(x){x$tpf.int.hat})) - true.lambda
  bias.re.est <- unlist(lapply(dta.in.re, function(x){x$re.int.hat})) - true.lambda
  
  cov.tpf.est <- unlist(lapply(dta.in.tpf, function(x){x$coverage.tpf.int})) 
  cov.re.est <- unlist(lapply(dta.in.re, function(x){(as.numeric(log(true.lambda) >= log(x$re.int.hat) - 1.96*sqrt(x$var.log.re.int)) & (log(true.lambda) <= log(x$re.int.hat) + 1.96*sqrt(x$var.log.re.int)))}))
  
  power.tpf.est <- unlist(lapply(dta.in.tpf, function(x){x$pvals.tpf.int})) < 0.05
  power.re.est <- unlist(lapply(dta.in.re, function(x){x$pvals.log.re.int})) < 0.05
  
  out <- data.frame(bias.tpf.est, bias.re.est,
                    cov.tpf.est, cov.re.est,
                    power.tpf.est, power.re.est)
  return(out)
}

old.performance.function <- function(dta.re, dta.tpf, true.rr){
  bias.tpf.est <- unlist(dta.tpf[["lambdas"]]) - true.rr
  bias.re.est <- unlist(dta.re[["OR_me"]]) - true.rr
  
  cov.tpf.est <- unlist(dta.tpf[["coverage"]])
  cov.re.est <- log(true.rr) <= log(unlist(dta.re[["OR_me"]])) + 1.96*unlist(dta.re[["sd_me_est"]]) &
    log(true.rr) >= log(unlist(dta.re[["OR_me"]])) - 1.96*unlist(dta.re[["sd_me_est"]])
  
  power.tpf.est <- unlist(dta.tpf[["pVals"]]) <= 0.05
  power.re.est <- unlist(dta.re[["pval_me"]]) <= 0.05
  
  out <- data.frame(bias.tpf.est, bias.re.est,
                    cov.tpf.est, cov.re.est,
                    power.tpf.est, power.re.est)
  return(out)
}

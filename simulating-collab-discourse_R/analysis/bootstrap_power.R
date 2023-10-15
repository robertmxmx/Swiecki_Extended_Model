#bootstrap analysis to determine the estimated power
#of the simulation validation test
#see Kleinman, K., & Huang, S. S. (2016). 
#Calculating power by bootstrap, with an application to cluster-randomized trials. 
#EGEMs, 4(1).

#h_0 = d < d*_95
#h_a = d > d*_95

sim_boot_p = function(test_stat,null_dist){
  ##diff for alt hypothesis
  #browser()
  val = quantile(null_dist,c(0.95))
  diff = test_stat - val
  ##boot ids
  samp = sample(x = 1:length(null_dist),
                size = length(null_dist),
                replace = TRUE)
  ##bootstrap distribution
  boot_dist = null_dist[samp]
  boot_dist = boot_dist - abs(diff)
  ##p val
  x = sum(boot_dist >= test_stat)
  p = x/length(boot_dist)
  
  return(p)
}

sim_pwr_boot = function(test_stat,null_dist,n){
  #get all pvals
  p_vals = replicate(n = n,
                     expr = sim_boot_p(test_stat,null_dist),
                     simplify = FALSE)
  
  #find number of rejections
  sig = which(unlist(p_vals) < 0.05)
  #find proportion of rejections
  pwr = length(sig)/length(p_vals)
  
  return(list(power = pwr, dist = unlist(p_vals)))
}


###testing
  
#sim_val_res = sim_trace_validation_final_results$test
# 
# sim_boot_p(test_stat = sim_val_res$statistic,
#            null_dist = sim_val_res$distribution)
# 
#test = sim_pwr_boot(test_stat = sim_val_res$statistic,
                    null_dist = sim_val_res$distribution,
                    n = 1000)


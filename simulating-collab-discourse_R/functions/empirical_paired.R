#function for running an empirical paired sample t.test

empirical_paired = function(obs,sim){
  #calculate the mean of the distribution of models (i.e., the centroid of the 
  #point cloud)
  #browser()
  meanMod = Reduce("+",sim)/length(sim)
  
  # calculate difference between each observed point and the centroid.
  obs_diffs = obs - meanMod
  mean_obs_diffs = mean(obs_diffs)
  
  # calculate difference between each simulated point and the centroid.
 sim = do.call("cbind",sim)
 sim_diffs = apply(X = sim,MARGIN = 2,FUN = function(x) x - meanMod)
 mean_sim_diffs = colMeans(sim_diffs)
  
  #get confidence interval for null hypothesis distribution
  ci = quantile(mean_sim_diffs,c(.25,0.975))
  
  return(list(ci = ci, obs_mean = mean_obs_diffs))
}
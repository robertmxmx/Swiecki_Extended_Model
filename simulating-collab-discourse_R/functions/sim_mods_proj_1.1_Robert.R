#function for returning inter mods from sim data (projected in to real space)
#also calculates soc/cog sym on simulated data



sim_mods_proj_1.1 = function(codes,
                    trans_mat,
                    adj_mats,
                    window_size_inter,
                    proj_set,
                    type,
                    steps,
                    version,
                    iterative_mean){
  #browser()
  
  ##simulate data
  dat_ = sim_discourse_3.1(trans_mat = trans_mat,
                           adj_mats = adj_mats,
                           type = type,
                           steps = steps,
                           version = version,
                           iterative_mean = iterative_mean)
  
  return (dat_)

}

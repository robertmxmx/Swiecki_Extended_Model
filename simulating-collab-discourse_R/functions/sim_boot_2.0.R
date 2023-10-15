#script for bootstraping transition matrices and code matrices and producing models
##r = number of replicates
##teams = number of teams in a given simulated dataset
##window = window size for inter model
##steps = number of lines per team



sim_boot.2.0 = function(codes, 
                        window, 
                        t_matrices,
                        adj_mats,
                        type,
                        normalize){

#get metrics for sampled matrices ###Need to update to run on the simulated data and not the matrices
# ts = map(t_matrices,pluck,"mat")
# t_matrix_metrics_sample = lapply(ts,interactivity)
# t_matrix_metrics_sample = bind_rows(t_matrix_metrics_sample,.id = "id")
# c_matrix_metrics_sample = lapply(c_matrices,cog_sym_ind)
# c_matrix_metrics_sample = bind_rows(c_matrix_metrics_sample,.id = "id")
# matrix_metrics_sample = left_join(t_matrix_metrics_sample,
#                                   c_matrix_metrics_sample,
#                                   by = c("id","speaker"))

#simulate dataset based on sampled matrices
sim_dists = map2(.x = t_matrices,
                 .y = adj_mats,
                 .f = sim_dists,
                 codes = codes,
                 window_size_inter = window,
                 type = type,
                 normalize = normalize) 

#browser()


#matrix_metrics_sample$dists = unlist(sim_dists)

#run model
#ICCest(x = id,y = dists, data = matrix_metrics_sample) #groups do not matter
#mod = lmer(dists ~ 1 + interactivity.ind + sims + interactivity.ind*sims + (1|id),data = matrix_metrics_sample)#can't use lmer bc the observed and simulated data will not have the same grouping variables
#mod = lm(dists ~ 1 + interactivity.ind + sims + interactivity.ind*sims, data = matrix_metrics_sample)
#extract coefficients
#mod.info = tidy(mod)
#betas = mod$estimate[mod$effect == "fixed"]
#betas = mod.info$estimate
#names(betas) = c("Intercept","interactivity","similarity","interaction")
#return(list(model = mod, coefs = betas))
return(unlist(sim_dists))
}
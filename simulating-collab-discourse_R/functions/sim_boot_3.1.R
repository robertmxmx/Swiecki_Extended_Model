#script for bootstraping transition matrices and code matrices and producing models
##r = number of replicates
##teams = number of teams in a given simulated dataset
##window = window size for inter model
##steps = number of lines per team
##model based not impact of interdependence based




sim_boot.3.1 = function(codes, 
                        window, 
                        t_matrices,
                        adj_mats,
                        type,
                        steps,
                        proj_set){


#simulate dataset based on sampled matrices
sim_inter_mods = map2(.x = t_matrices,
                 .y = adj_mats,
                 .f = sim_mods_proj_1.1,
                 codes = codes,
                 window_size_inter = window,
                 type = type,
                 steps = steps,
                 proj_set = proj_set)


sim_inter_mods = bind_rows(sim_inter_mods,.id = "id")

#run model
mod = lmer(MR1 ~ 1 + cog.sym.ind*soc.sym.ind + cog.mean + soc.mean + (1|id),data = sim_inter_mods)

#extract coefficients

mod.info = tidy(mod)
betas = mod.info$estimate[mod.info$effect == "fixed"]
names(betas) = mod.info$term[mod.info$effect == "fixed"]
return(list(coefs = betas, model = mod))
}
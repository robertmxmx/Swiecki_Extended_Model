#script for bootstraping transition matrices and code matrices and producing models
##r = number of replicates
##teams = number of teams in a given simulated dataset
##window = window size for inter model
##steps = number of lines per team
##model based not impact of interdependence based

##just generates the data
##run regression after

sim_boot.4.0 = function(codes, 
                        window, 
                        t_matrices,
                        adj_mats,
                        type,
                        steps,
                        proj_set){


#simulate dataset based on sampled matrices
sim_inter_mods = map2(.x = t_matrices,
                 .y = adj_mats,
                 .f = sim_mods_proj,
                 codes = codes,
                 window_size_inter = window,
                 type = type,
                 steps = steps,
                 proj_set = proj_set)


sim_inter_mods = bind_rows(sim_inter_mods,.id = "Team")
return(sim_inter_mods)
}
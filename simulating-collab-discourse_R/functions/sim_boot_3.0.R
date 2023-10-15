#script for bootstraping transition matrices and code matrices and producing models
##r = number of replicates
##teams = number of teams in a given simulated dataset
##window = window size for inter model
##steps = number of lines per team
##model based not impact of interdependence based



sim_boot.3.0 = function(codes, 
                        window, 
                        t_matrices,
                        adj_mats,
                        type,
                        normalize,
                        steps){



#simulate dataset based on sampled matrices
sim_inter_mods = map2(.x = t_matrices,
                 .y = adj_mats,
                 .f = sim_mods,
                 codes = codes,
                 window_size_inter = window,
                 type = type,
                 normalize = normalize,
                 steps = steps) 


return(sim_inter_mods)
}
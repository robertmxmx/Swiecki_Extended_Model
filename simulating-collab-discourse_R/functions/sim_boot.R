#script for bootstraping transition matrices and producing models
##r = number of replicates
##speakers = number of speakers
##teams = number of teams in a given simulated dataset
##window = window size for inter model
##steps = number of lines per team

##NEED TO UPDATE TO CALCULATE THE MATRIX METRICS ON THE ACTUAL DATA

sim_boot = function(speakers, 
                    codes, 
                    window, 
                    steps,
                    matrices){

#get metrics for sampled matrices
matrix_metrics_sample = lapply(matrices,interactivity)
matrix_metrics_sample = bind_rows(matrix_metrics_sample,.id = "id")

#simulate dataset based on sampled matrices
sim_dists = lapply(matrices,
                   sim_dists, 
                   speakers = speakers,
                   codes = codes,
                   steps = steps,
                   window_size_inter = window)

matrix_metrics_sample$dists = unlist(sim_dists)

#run model
#ICCest(x = id,y = dists, data = matrix_metrics_sample) #groups do not matter
mod = lm(dists ~ 1 + interactivity.ind,data = matrix_metrics_sample)
#extract coefficients
betas = coef(mod)

return(betas)
}
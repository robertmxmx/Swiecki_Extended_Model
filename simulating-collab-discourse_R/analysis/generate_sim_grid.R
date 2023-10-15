#Matrix for each person adj_mat_list_boot
#Speaker Matrix t_matrix_dist

#script for generating simulation grid--the joint distribution
#of cognitive and social symmetry matrices used in the simulation

#load libraries
library(rENA)
library(magrittr)
library(markovchain)
library(tidyverse)

#source functions
source('~/Rprojects/simulating-collab-discourse/functions/generate_speaker_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/interactivity.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_speaker_sequence.R')
source('~/Rprojects/simulating-collab-discourse/functions/soc.sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/update_trans_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/gen_code_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/cog_sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/gen_adj_matrix.R')

#load data
source('~/Rprojects/simulating-collab-discourse/analysis/real_data_results_final.R')
load("~/Rprojects/simulating-collab-discourse/data/co_dists.Rdata")

set.seed(1234)

#generate the simulation grid
##generate transition matrices
t_matrix_dist = replicate(n = 5,expr = gen_speaker_matrix(5),simplify = FALSE)
t_matrix_ids = sample(c(1:length(t_matrix_dist)),size = 200,replace = TRUE)
t_matrix_sample = t_matrix_dist[t_matrix_ids]

##generate adj matrices for each individual on each team
###random draw from observed distribution of co-occurrences, then jitter 
factor.list =  seq.int(1,200,5) #for jittering 
adj_mat_list_jit = list()
for (i in 1:length(factor.list)){
  adj_mat_list_jit[[i]] = replicate(n = 25,
                                    expr = gen_adj_matrix_team(ncodes = 9,
                                                               nspeakers = 5,
                                                               distribution = co_dists,
                                                               noise = factor.list[i]),
                                    simplify = FALSE)
}
adj_mat_list_jit = flatten(adj_mat_list_jit)

###bootstrap real matrices
real_mats = co_mats_team
real_mats = real_mats[-c(5,6)]
boot_ids = sample(c(1:8),size = 10,replace = TRUE)

adj_mat_list_boot = list()
for(i in 1:length(boot_ids)){
  adj_mat_list_boot[[i]] = real_mats[[boot_ids[i]]]
}

##random draw from observed distribution of co-occurrences
adj_mat_list_rand = replicate(n = 10,
                              expr = gen_adj_matrix_team_nj(ncodes = 9,
                                                            nspeakers = 5,
                                                            distribution = co_dists),
                              simplify = FALSE)



##combine
adj_mat_list = list(adj_mat_list_jit,adj_mat_list_boot,adj_mat_list_rand)
adj_mat_list = flatten(adj_mat_list)

##sample
c_matrix_ids = sample(c(1:length(adj_mat_list)),size = 200,replace = TRUE)
c_matrix_sample = adj_mat_list[c_matrix_ids]

#view and check distributions of social and cognitive symmetry
##calc social
t_check_full = lapply(t_matrix_dist,interactivity) 
t_check_full = bind_rows(t_check_full)
t_check_full$type = rep("pop",nrow(t_check_full))
t_check = lapply(t_matrix_sample,interactivity)
t_check = bind_rows(t_check)
t_check$type = rep("sample",nrow(t_check))

##calc cog
c_check_full = lapply(adj_mat_list,cog_sym_adj) 
c_check_full = bind_rows(c_check_full)#do.call("rbind",c_check_full)
c_check_full$type = rep("pop",nrow(c_check_full))
c_check = lapply(c_matrix_sample,cog_sym_adj)
c_check = bind_rows(c_check)
c_check$type = rep("sample",nrow(c_check))

##plot social symmetry distributions
t_matrix_info = rbind(t_check_full, t_check)
p = ggplot(aes(x = interactivity.ind, fill = type),data = t_matrix_info) +
  geom_density(alpha = 0.2, position = "identity") +
  xlim(c(0,1)) + ggtitle("Speakers")

##plot cognitive symmetry distributions
c_matrix_info = rbind(c_check_full, c_check)
p2 = ggplot(aes(x = avgdists, fill = type),data = c_matrix_info) +
  geom_density(alpha = 0.2, position = "identity")+ 
  ggtitle("Codes") #+ xlim(0,1.5)

##plot joint distribution from sample
both_mat = cbind(t_check,c_check)
both_mat = both_mat[,c(1,2,4,5)]
p3 = ggplot(aes(x = avgdists, y = interactivity.ind), data = both_mat) +
  geom_point() + ggtitle("Joint Distribution",subtitle = "Simulation") + ylim(0,1)+ xlim(0,4)+ xlab(label = "dissimilarity") + ylab(label = "interactivity") + theme_minimal()

##plot joint distribution from real
p4 = ggplot(aes(x = dissimilarity, y = interactivity), data = dat_) +
  geom_point() + ggtitle("Joint Distribution",subtitle = "Observed") + ylim(0,1)+ xlim(0,4)+ xlab(label = "dissimilarity") + ylab(label = "interactivity") + theme_minimal()


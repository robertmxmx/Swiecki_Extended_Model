#validation test (using empirical distance tests)
#rm(list = ls())

#libraries
library(rENA)
library(magrittr)
library(markovchain)
library(tidyverse)
library(ICC)
library(lmerTest)
library(interactions)
library(matlab)
library(truncnorm)
library(broom)
library(broom.mixed)
library(coxed)

#functions
source('~/Rprojects/simulating-collab-discourse/functions/sim_discourse_3.1.R')
source('~/Rprojects/simulating-collab-discourse/functions/generate_speaker_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/interactivity.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_speaker_sequence.R')
source('~/Rprojects/simulating-collab-discourse/functions/cog.sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/soc.sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/distance.R')
source('~/Rprojects/simulating-collab-discourse/functions/update_trans_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_dists.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_boot_3.0.R')
source('~/Rprojects/simulating-collab-discourse/functions/gen_code_matrix.R')
source('~/Rprojects/simulating-collab-discourse/functions/cog_sym.R')
source('~/Rprojects/simulating-collab-discourse/functions/matrices_from_observed.R')
source('~/Rprojects/simulating-collab-discourse/functions/empirical_distance_test.R')
source('~/Rprojects/simulating-collab-discourse/functions/remove.silent.R')
source('~/Rprojects/simulating-collab-discourse/functions/empirical_paired.R')
source('~/Rprojects/simulating-collab-discourse/functions/remove_missing_units.R')
source('~/Rprojects/simulating-collab-discourse/functions/sim_mods.R')
source('~/Rprojects/simulating-collab-discourse/functions/ena.compare.models.R')
source('~/Rprojects/simulating-collab-discourse/functions/distance.R')

#load data
source('~/Rprojects/simulating-collab-discourse/analysis/real_data_results_final.R')

#split data by team
team.split = split(rs.fg,rs.fg$GroupName)

##get cognitive and social matrices from real data
###social 

trans_mats = lapply(team.split,
                    get_trans_mat,
                    speakerCol = "UserName",
                    teamCol = "GroupName")



###cognitive
co_mats_team = co_mats_team[sort(names(co_mats_team))]

#run simulation using these matrices
set.seed(2)

mod.results = replicate(n = 1, expr = sim_boot.3.0(
  codes = 8,
  window = 2,
  t_matrices = trans_mats,
  adj_mats = co_mats_team,
  type = "real", 
  normalize = TRUE,
  steps = null), simplify = FALSE)


#extract simulated adjacency vectors
mod.results = map(mod.results,bind_rows,.id = "id")

#add ids
add_rnames = function(x){
  rnames = paste(x$id,x$ENA_UNIT,sep = ".")
  rownames(x) = rnames
  return(x)
}

mod.results = map(mod.results,add_rnames)



#get real model
##get metadata
correct.meta = set.inter$meta.data %>% arrange(ENA_UNIT)

##arrange data
real_mod = set.inter$line.weights %>% 
              arrange(ENA_UNIT) %>% 
              as.matrix()
##bind with metadata
real_mod = cbind(correct.meta,real_mod)

##re-order to align with simulated results
real_mod = real_mod %>% arrange(GroupName,UserName)
##restructure to be team list
team.counts = correct.meta %>% group_by(GroupName) %>% count()
names.list = list()
for (i in 1:nrow(team.counts)){
  group = team.counts[i,1]
  unitno = team.counts[i,2]
  group = rep(group, unitno)
  unitno = seq.int(1:as.numeric(unitno))
  ids = cbind(group,unitno)
  names.list[[i]] = ids
}
ids = do.call("rbind",names.list)
ids = data.frame(ids)
rownames(ids) = NULL
ids$id = paste(ids$group,ids$unitno,sep = ".")
rownames(real_mod) = ids$id

#find missing units from the simulation--those who ended up with no connections in a given run
full.names = rownames(real_mod)
missing.units = map(mod.results,get.silent.mod,all.names = full.names)
missing.units = discard(missing.units,function(x)length(x)==0)
missing.units = unique(unlist(missing.units))

#remove missing units from real and simulated data
real_mod = remove_missing_units(real_mod,missing.units,"real")
mod.results = map(mod.results,remove_missing_units,missing.units,"real")


#check that real and simulated data have the same number of units
unit.counts = map(mod.results,nrow)
real.count = nrow(real_mod)

if(all(unlist(unit.counts)==real.count)){
  print("all lists have equal counts")
}else{
  print("you messed up")
}

#compare using empirical distance test.
##See 
##Swiecki, Z. (in press). 
##The expected value test: A new statistical warrant for theoretical saturation. 
##Paper submitted to the Third International Conference on Quantitative Ethnography.

write.csv(real_mod, "C:/Users/granc/Documents/RProjects/simulating-collab-discourse/Real_mod.csv", row.names=FALSE)
write.csv(mod.results, "C:/Users/granc/Documents/RProjects/simulating-collab-discourse/Sim_mod.csv", row.names=FALSE)


mod_a = real_mod %>% select(contains("&"))

mod_b = map(mod.results,select,contains("V"))
mod_b = map(mod_b,data.frame)

print(mod_a)
print(mod_b)

write.csv(mod_a, "C:/Users/granc/Documents/RProjects/simulating-collab-discourse/Real_mod_a.csv", row.names=FALSE)
write.csv(mod_b, "C:/Users/granc/Documents/RProjects/simulating-collab-discourse/Sim_mod_b.csv", row.names=FALSE)

test = ena.compare.models(observedMod = mod_a,
                          simMods = mod_b,
                          method = "euclidean")
print(test)
#calculate bias corrected, accelerated percentile intervals
test.ci = bca(test$distribution)
print(test.ci)

